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
use common::{assert_file_cst_ok, document_values, parse_clean, render};
use proptest::prelude::*;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::custom_tags::{
    CompressedBinaryHandler, CustomTagError, CustomTagHandler, CustomTagRegistry, EnvVarHandler,
    JsonHandler, TimestampHandler,
};
use yaml_edit::validator::Validator;
use yaml_edit::{
    debug, Document, Entry, Mapping, OccupiedEntry, ScalarValue, SyntaxKind, YamlFile, YamlNode,
    YamlValue,
};

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

/// A tag or anchor with no node after it annotates an implicit null, and
/// does not adopt a dedented entry after it: the anchor after `!e` used to
/// adopt `f: 1` as its value.
#[test]
fn annotation_without_a_node() {
    assert_renders(&[
        (
            "\n- a: 1\n  b:\n    c:\n      d:\n      - !e\n  f:\n  - !g\n    h: 1\n  i:\n  - !j { }\n    ",
            "seq[map{a: 1, b: map{c: map{d: seq[!e ~]}}, f: seq[!g map{h: 1}], i: seq[!j {}]}]",
        ),
        ("- a:\n  - !e &x\n  f: 1\n", "seq[map{a: seq[!e ~], f: 1}]"),
        ("- a:\n  - &x\n  f: 1\n", "seq[map{a: seq[~], f: 1}]"),
    ]);
}

/// An annotated key after a nested value is the next entry. It used to be
/// read as a continuation of the value, giving the single value `b &x c: d`.
#[test]
fn annotated_key_after_nested_value() {
    assert_renders(&[
        ("k:\n  a: b\n  &x c: d\n", "map{k: map{a: b, c: d}}"),
        ("- a: b\n  &x c: d\n", "seq[map{a: b, c: d}]"),
        ("k:\n  a: \"b\"\n  !t c: d\n", "map{k: map{a: b, !t c: d}}"),
    ]);
}

/// An annotation in front of the document root does not change that the
/// root is a plain scalar, so its continuation lines need clear no column.
///
/// `" !x t\no\n"` is the tagged scalar `t o`, as saphyr reads it, exactly as
/// the untagged `" t\no\n"` is `t o`. The root-scalar check only looked at
/// the first token, which for an annotated root is the TAG or ANCHOR, so the
/// floor stayed at the annotation's own column and the continuation was
/// swept into an ERROR node while from_str still reported success. An
/// annotation on its own line opens a block node instead, so the relaxed
/// floor must not reach it.
#[test]
fn annotated_root_scalar_folds_a_less_indented_continuation() {
    assert_renders(&[
        (" !x t\no\n", "!x t o"),
        (" ! t\no\n", "! t o"),
        (" &a t\no\n", "t o"),
        ("  !x t\n b\n", "!x t b"),
        (" !x &a t\no\n", "!x t o"),
        (" &a !x t\no\n", "!x t o"),
        ("!x\n  a: 1\n", "!x map{a: 1}"),
    ]);
}

/// A tag says what the node beneath it means, so it does not stop the
/// document's root being reachable as that kind.
///
/// `"!!map\n a: 1\n"` is a mapping, as saphyr reads it, but as_mapping,
/// as_sequence and as_scalar all answered None for an annotated root, which
/// left the whole document unreachable through Document. The tag itself is
/// still reachable, and an untagged root is unaffected.
#[test]
fn annotated_document_root_is_reachable() {
    let file = YamlFile::from_str("!!map\n a: 1\n").unwrap();
    let doc = file.document().unwrap();
    assert_eq!(doc.as_mapping().unwrap().len(), 1);
    assert_eq!(doc.as_tagged().unwrap().tag().as_deref(), Some("!!map"));

    let file = YamlFile::from_str("!!seq\n- x\n- y\n").unwrap();
    let doc = file.document().unwrap();
    assert_eq!(doc.as_sequence().unwrap().len(), 2);

    let file = YamlFile::from_str("!!str v\n").unwrap();
    let doc = file.document().unwrap();
    assert_eq!(doc.as_scalar().unwrap().as_string(), "v");

    let file = YamlFile::from_str("a: 1\n").unwrap();
    let doc = file.document().unwrap();
    assert!(doc.as_tagged().is_none());
    assert_eq!(doc.as_mapping().unwrap().len(), 1);
}

fn occupied<'a>(mapping: &'a Mapping, key: &str) -> OccupiedEntry<'a> {
    match mapping.entry(key) {
        Entry::Occupied(o) => o,
        Entry::Vacant(_) => panic!("{key} missing"),
    }
}

/// A tag does not change what kind of node it annotates, so the typed
/// getters see through it the way Document::as_mapping and get_string do.
#[test]
fn typed_getters_see_through_tags() {
    let yaml = "a: !!map\n  x: 1\nb: &b !!map\n  y: 2\nc: !custom\n  z: 3\ns: !!seq\n  - 1\nt: !custom [1, 2]\n";
    let doc = YamlFile::from_str(yaml).unwrap().document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    for (key, inner) in [("a", "x"), ("b", "y"), ("c", "z")] {
        let nested = mapping.get_mapping(key).unwrap();
        assert!(nested.contains_key(inner), "{key}");
        assert!(doc.get_mapping(key).unwrap().contains_key(inner), "{key}");
        assert_eq!(occupied(&mapping, key).get_mapping().unwrap().len(), 1);
        assert!(mapping.modify_mapping(key, |m| assert!(m.contains_key(inner))));
    }
    assert_eq!(mapping.get_sequence("s").unwrap().len(), 1);
    assert_eq!(doc.get_sequence("t").unwrap().len(), 2);
    assert_eq!(occupied(&mapping, "t").get_sequence().unwrap().len(), 2);

    // The tag stays put when editing through the getter.
    mapping.get_mapping("c").unwrap().set("w", 4);
    common::assert_cst_ok(&doc);
    assert_eq!(
        doc.to_string(),
        "a: !!map\n  x: 1\nb: &b !!map\n  y: 2\nc: !custom\n  z: 3\n  w: 4\ns: !!seq\n  - 1\nt: !custom [1, 2]\n"
    );

    // A tagged node of another kind is still not a mapping.
    assert!(mapping.get_mapping("s").is_none());
    assert!(mapping.get_sequence("a").is_none());
}

/// A named tag handle and its suffix are one tag, and `#` is a URI
/// character, so it belongs to the tag.
#[test]
fn tag_spellings() {
    for (yaml, tag, value) in [
        ("%TAG !e! tag:x,2000:\n---\n!e!foo bar\n", "!e!foo", "bar"),
        ("!foo#bar x\n", "!foo#bar", "x"),
    ] {
        let file = parse_clean(yaml);
        let doc = file.documents().next().unwrap();
        let tagged = doc.as_tagged().unwrap();
        assert_eq!(tagged.tag().unwrap(), tag, "{yaml:?}");
        assert_eq!(tagged.value().unwrap().as_string(), value, "{yaml:?}");
    }
}

/// A tagged value may be a block scalar, whose raw text keeps its header
/// and indentation.
#[test]
fn tagged_block_scalars() {
    for (yaml, key, tag, raw) in [
        (
            "\nmessage: !custom |\n  This is a multi-line\n  custom tagged message\n  with preserved formatting\n",
            "message",
            "!custom",
            "|\n  This is a multi-line\n  custom tagged message\n  with preserved formatting\n",
        ),
        (
            "\ndescription: !note >\n  This is a long\n  description that\n  should be folded\n  into a single line.\n",
            "description",
            "!note",
            ">\n  This is a long\n  description that\n  should be folded\n  into a single line.\n",
        ),
    ] {
        let file = parse_clean(yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let tagged = mapping.get(key).unwrap();
        let tagged = tagged.as_tagged().unwrap();
        assert_eq!(tagged.tag().as_deref(), Some(tag));
        assert_eq!(tagged.value().unwrap().value(), raw);
    }
}

#[test]
fn binary_data_in_a_block_scalar() {
    for (yaml, key, expected) in [
        (
            r#"
data: !!binary |
  R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
  OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
  +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
  AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=
"#,
            "data",
            "|R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/++f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLCAgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=",
        ),
        (
            r#"image: !!binary |
  iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhf
  DwAChwGA60e6kgAAAABJRU5ErkJggg==
"#,
            "image",
            "|iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mNkYPhfDwAChwGA60e6kgAAAABJRU5ErkJggg==",
        ),
    ] {
        let file = parse_clean(yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let data = mapping.get(key).unwrap();
        let tagged = data.as_tagged().unwrap();
        assert_eq!(tagged.tag(), Some("!!binary".to_string()));
        let content = tagged.value().unwrap().value();
        let normalized: String = content.chars().filter(|c| !c.is_whitespace()).collect();
        assert_eq!(normalized, expected);
    }
}

/// A plain scalar tagged `!!omap`, `!!pairs` or `!!set` parses as a tagged
/// scalar, as with any other tag; the validator reports that it is not the
/// collection the tag names. A block mapping cannot start on its tag's line.
///
/// The collection parser found nothing to take, leaving an empty node, and
/// the scalar was dropped into an ERROR node with no error reported at all.
/// A scalar on the tag's own line was then made a parse error, while one on
/// the next line (`!!omap\n  3\n`) parsed without one.
#[test]
fn collection_tag_on_a_scalar() {
    for (tag, yaml, message, location) in [
        (
            "!!omap",
            "!!omap 3",
            "!!omap content is not a sequence",
            "1:1",
        ),
        (
            "!!pairs",
            "!!pairs a",
            "!!pairs content is not a sequence",
            "1:1",
        ),
        ("!!set", "!!set x", "!!set content is not a mapping", "1:1"),
        (
            "!!omap",
            "!!omap\n  3\n",
            "!!omap content is not a sequence",
            "1:1",
        ),
        (
            "!!set",
            "!!set ? a\n",
            "A block mapping cannot start on the line of its tag or anchor",
            "1:7",
        ),
    ] {
        let file = common::parse_with_violations(yaml, &[(message, location)]);
        let plain = YamlFile::from_str(&yaml.replace(tag, "!x")).unwrap();
        assert_eq!(
            debug::tree_to_string(file.syntax()).replace(tag, "!x"),
            debug::tree_to_string(plain.syntax()),
            "{yaml:?}"
        );
    }
}

/// The collection forms of those tags are unaffected, nested too. A `?`
/// line in a `!!set` nested inside another mapping was read as a
/// continuation of the previous key's plain scalar, giving the single key
/// `a ? b`: the set's mapping is parsed by parse_mapping_with_base_indent,
/// whose `?` branch kept the enclosing value's continuation context.
#[test]
fn collection_tags() {
    assert_renders(&[
        ("!!omap\n- a: 1\n", "!!omap seq[map{a: 1}]"),
        ("!!pairs\n- a: 1\n", "!!pairs seq[map{a: 1}]"),
        ("!!set\n? a\n", "!!set map{a: ~}"),
        ("!!omap []\n", "!!omap []"),
        (
            "outer:\n  k: !!set\n    ? a\n    ? b\n",
            "map{outer: map{k: !!set map{a: ~, b: ~}}}",
        ),
        (
            "outer:\n  k: !!set\n    ? a\n    : 1\n    ? b\n    : 2\n",
            "map{outer: map{k: !!set map{a: 1, b: 2}}}",
        ),
        (
            "outer:\n  k: !!set\n    ? a\n    ? b\n  z: 1\n",
            "map{outer: map{k: !!set map{a: ~, b: ~}, z: 1}}",
        ),
        (
            "outer:\n  k: !!set\n    ? !t\n    : v\n    ? b\n",
            "map{outer: map{k: !!set map{!t ~: v, b: ~}}}",
        ),
    ]);
}

/// `!!set`, `!!omap` and `!!pairs` on a collection of the other shape
/// parse like any other tag; whether the content suits the tag is for the
/// validator to say.
///
/// The parser chose the collection shape from the tag, so `!!set [a, b]`
/// went to the block mapping parser, which took nothing and left the flow
/// sequence unparsed.
#[test]
fn collection_tag_on_other_collection_shape() {
    for (tag, yaml) in [
        ("!!set", "!!set [a, b]\n"),
        ("!!set", "!!set &x [a]\n"),
        ("!!set", "x: !!set [a, b]\ny: 1\n"),
        ("!!set", "!!set\n- a\n- b\n"),
        ("!!set", "!!set\n  - a\n"),
        ("!!set", "k: !!set\n- a\nj: 1\n"),
        ("!!omap", "!!omap {a: 1}\n"),
        ("!!pairs", "!!pairs {a: 1}\n"),
        ("!!omap", "!!omap\n  a: 1\n"),
        ("!!pairs", "!!pairs\na: 1\nb: 2\n"),
        ("!!set", "!!set\n# c\n- a\n"),
        ("!!set", "!!set # c\n\n- a\n"),
        ("!!set", "- k: !!set\n  - a\n- j\n"),
        ("!!omap", "!!omap\n\n  a: 1\n"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let plain = YamlFile::from_str(&yaml.replace(tag, "!x")).unwrap();
        assert_eq!(
            debug::tree_to_string(file.syntax()).replace(tag, "!x"),
            debug::tree_to_string(plain.syntax()),
            "{yaml:?}"
        );
    }
    // With no content after the line break, the tag annotates an empty
    // node, as any other tag does.
    for (yaml, expected) in [
        ("!!omap\n", "SCALAR"),
        ("!!pairs\n# c\n", "SCALAR"),
        ("!!omap\n...\n", "SCALAR"),
        ("!!set\n", "SCALAR"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let tagged = file
            .syntax()
            .descendants()
            .find(|n| n.kind() == SyntaxKind::TAGGED_NODE)
            .unwrap();
        let content: Vec<String> = tagged
            .children()
            .map(|n| format!("{:?}", n.kind()))
            .collect();
        assert_eq!(content, [expected], "{yaml:?}");
    }
    // Nor does a sibling entry become the body.
    for yaml in [
        "- !!set\n- b\n",
        "- !!omap\n- a: 1\n",
        "- !!pairs\n- a: 1\n",
        "- !!omap &o\n- a: 1\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let sequence = file.document().unwrap().as_sequence().unwrap();
        assert_eq!(sequence.len(), 2, "{yaml:?}");
        let first = sequence.get(0).unwrap();
        let tagged = first.as_tagged().unwrap();
        assert_eq!(
            tagged
                .syntax()
                .children()
                .map(|n| n.kind())
                .collect::<Vec<_>>(),
            [SyntaxKind::SCALAR],
            "{yaml:?}"
        );
    }
    // A block mapping in the value position still has to nest under its
    // key; at the key's column its entries are siblings.
    for yaml in [
        "k: !!set\na: 1\n",
        "k: !!omap\na: 1\n",
        "k: !!pairs\na: 1\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping.keys().map(|k| k.to_string()).collect();
        assert_eq!(keys, ["k", "a"], "{yaml:?}");
    }
}

/// A tagged flow collection is an implicit key like an untagged one,
/// whatever the tag.
#[test]
fn tagged_flow_collection_as_implicit_key() {
    for yaml in [
        "!!set {a}: 1\n",
        "!!omap [a: 1]: 1\n",
        "!!seq [a]: 1\n",
        "k:\n  !!set {a}: 1\n",
        "- !!pairs [a: 1]: 1\n",
    ] {
        let file = parse_clean(yaml);
        let key = file
            .syntax()
            .descendants()
            .find(|n| n.kind() == SyntaxKind::KEY && n.text().to_string().starts_with("!!"))
            .unwrap();
        assert_eq!(
            key.children().map(|n| n.kind()).collect::<Vec<_>>(),
            [SyntaxKind::TAGGED_NODE],
            "{yaml:?}"
        );
    }
}

/// A `-` on the line after an anchor or tag that ends a compact sequence
/// entry is that sequence's next entry when it is at the entry's dash, not
/// the annotated node. `- - &a\n  - x\n` nested `x` under the anchor, so
/// pushing onto `[&a null]` wrote text that read back as a different tree.
#[test]
fn entry_after_an_annotated_compact_entry() {
    for yaml in ["- - &a\n  - x\n", "- - !t\n  - x\n"] {
        let doc = Document::from_str(yaml).unwrap();
        let inner = doc.as_sequence().unwrap().get(0).unwrap();
        assert_eq!(inner.as_sequence().unwrap().len(), 2, "{yaml:?}");
    }

    let doc = Document::from_str("- - &a\n").unwrap();
    let inner = doc.as_sequence().unwrap().get(0).unwrap();
    inner.as_sequence().unwrap().push("x");
    assert_eq!(doc.to_string(), "- - &a\n  - x\n");
    common::assert_parser_shaped(&doc);
}

/// An annotation at the very end of the input annotates the same zero-width
/// null it does when a line break follows it.
#[test]
fn annotation_at_end_of_input_holds_implicit_null() {
    for (yaml, expected) in [
        (
            "!!str",
            "TAGGED_NODE\n  TAG: \"!!str\"\n  SCALAR\n    NULL: \"\"\n",
        ),
        (
            "- &a",
            "SEQUENCE\n  SEQUENCE_ENTRY\n    DASH: \"-\"\n    WHITESPACE: \" \"\n    ANCHOR: \"&a\"\n    SCALAR\n      NULL: \"\"\n",
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let document = file.document().unwrap();
        assert_eq!(
            debug::tree_to_string(document.syntax()),
            expected
        );
    }
    let file = YamlFile::from_str("a: !!str").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let value = mapping.get("a").unwrap();
    let value = value.as_tagged().unwrap();
    assert_eq!(value.tag().unwrap(), "!!str");
    assert!(value.value().unwrap().is_null());
}

/// YAML test suite WZ62: a tag with no content after it in a flow
/// collection annotates an empty scalar. The key used to keep the text
/// `!!str ` (tag and the space before the colon) as its value.
#[test]
fn tagged_empty_flow_node() {
    let yaml = "{\n  foo : !!str,\n  !!str : bar,\n}\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();

    let value = mapping.get("foo").expect("Should have a key 'foo'");
    let value = value.as_tagged().expect("Should be a tagged node");
    assert_eq!(value.tag().unwrap(), "!!str");
    assert!(value.value().unwrap().is_null());
    assert_eq!(value.to_string(), "!!str");

    let keys: Vec<_> = mapping.keys().collect();
    assert_eq!(keys.len(), 2);
    let key = keys[1].as_tagged().expect("Should be a tagged node");
    assert_eq!(key.tag().unwrap(), "!!str");
    assert!(key.value().unwrap().is_null());
    assert_eq!(key.as_string().unwrap(), "");
    assert_eq!(key.to_string(), "!!str");

    let value = YamlValue::from_document(&file.document().unwrap()).unwrap();
    let value = value.as_mapping().unwrap();
    assert_eq!(value.keys().collect::<Vec<_>>(), vec!["", "foo"]);
    assert_eq!(value[""].as_str(), Some("bar"));
    assert_eq!(value["foo"].as_str(), Some(""));
}

/// The same holds for an anchor and in flow sequences: the annotation is
/// followed directly by a zero-width null, and the space after it stays
/// outside the annotated node.
#[test]
fn annotated_empty_flow_node() {
    use SyntaxKind;

    for (yaml, annotation) in [
        ("{ !!null : bar }", "!!null"),
        ("[ !!str , a ]", "!!str"),
        ("[ !!str ]", "!!str"),
        ("[ !!str\n]", "!!str"),
        ("[ !!str # c\n]", "!!str"),
        ("{ !!str\n  : bar }", "!!str"),
        ("{ a: !!str }", "!!str"),
        ("{ &x : bar }", "&x"),
        ("[ &x , a ]", "&x"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let token = file
            .syntax()
            .descendants_with_tokens()
            .filter_map(|e| e.into_token())
            .find(|t| t.text() == annotation)
            .unwrap();
        let next = token.next_sibling_or_token().unwrap();
        assert_eq!(next.kind(), SyntaxKind::SCALAR, "{yaml}");
        let null: Vec<_> = next
            .as_node()
            .unwrap()
            .children_with_tokens()
            .map(|e| (e.kind(), e.into_token().unwrap().text().to_string()))
            .collect();
        assert_eq!(null, vec![(SyntaxKind::NULL, String::new())], "{yaml}");
        if token.kind() == SyntaxKind::TAG {
            assert_eq!(token.parent().unwrap().text().to_string(), annotation);
        }
    }
}

#[test]
fn anchor_alone_on_value_line_before_sibling() {
    // The anchor annotates an empty value: `b` at the key's column is a
    // sibling entry, not the anchored node.
    let null = || YamlValue::Scalar(ScalarValue::parse(""));
    let mapping = |entries: Vec<(&str, YamlValue)>| {
        YamlValue::from_mapping(
            entries
                .into_iter()
                .map(|(k, v)| (k.to_string(), v))
                .collect(),
        )
    };
    let one = || YamlValue::from(1i64);
    assert_eq!(
        document_values("a:\n &x\nb: 1\n"),
        [mapping(vec![("a", null()), ("b", one())])]
    );
    assert_eq!(
        document_values("k:\n  a:\n   &x\n  b: 1\n"),
        [mapping(vec![(
            "k",
            mapping(vec![("a", null()), ("b", one())])
        )])]
    );
    // A more indented mapping, or a sequence at the key's column, is still
    // the anchored value.
    assert_eq!(
        document_values("a:\n &x\n b: 1\n"),
        [mapping(vec![("a", mapping(vec![("b", one())]))])]
    );
    assert_eq!(
        document_values("a:\n &x\n- 1\n"),
        [mapping(vec![("a", YamlValue::from(vec![1i64]))])]
    );
}
