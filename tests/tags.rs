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
use common::{
    assert_file_cst_ok, document_values, edit_metadata, parse_clean, render, render_documents,
};
use proptest::prelude::*;
use rowan::ast::AstNode;
use std::str::FromStr;
use std::sync::Arc;
use yaml_edit::custom_tags::{
    CompressedBinaryHandler, CustomTagError, CustomTagHandler, CustomTagRegistry, EnvVarHandler,
    JsonHandler, TimestampHandler,
};
use yaml_edit::validator::Validator;
use yaml_edit::{
    debug, CustomTagParser, Document, Entry, Mapping, MappingEntry, OccupiedEntry, ScalarValue,
    SyntaxKind, TaggedNode, YamlFile, YamlNode, YamlValue,
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

/// A tag on a value, rather than a key, still wraps the value after it.
#[test]
fn tag_annotated_value_still_wraps() {
    assert_eq!(render_documents("c: !!int 42\n"), ["map{c: !!int 42}"]);
}

#[test]
fn set_is_empty() {
    let file = YamlFile::from_str("a: !!set {}\nb: !!set {x}\n").unwrap();
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    let set = |key: &str| {
        mapping
            .get(key)
            .unwrap()
            .as_tagged()
            .unwrap()
            .as_set()
            .unwrap()
    };
    assert!(set("a").is_empty());
    assert_eq!(set("a").len(), 0);
    assert!(!set("b").is_empty());
    assert_eq!(set("b").len(), 1);
}

#[test]
fn tag_registry_basic() {
    let mut registry = CustomTagRegistry::new();

    // Register a handler
    let handler = JsonHandler;
    assert!(registry.register("!json", handler).is_ok());

    // Check if registered
    assert!(registry.has_tag("!json"));
    assert_eq!(registry.registered_tags(), vec!["!json"]);

    // Unregister
    assert!(registry.unregister("!json"));
    assert!(!registry.has_tag("!json"));
}

#[test]
fn json_handler() {
    let handler = JsonHandler;
    assert_eq!(
        handler.serialize(&YamlValue::scalar("test")),
        Ok("\"test\"".to_string())
    );
    assert_eq!(
        handler.serialize(&YamlValue::sequence()),
        Ok("[]".to_string())
    );
    assert_eq!(
        handler.deserialize("\"hello\""),
        Ok(YamlValue::scalar("hello"))
    );
    assert_eq!(handler.deserialize("plain"), Ok(YamlValue::scalar("plain")));
    assert_eq!(handler.validate("\"valid\""), Ok(()));
}

#[test]
fn json_handler_deserialize_collections() {
    let handler = JsonHandler;
    let num = |s: &str| YamlValue::Scalar(ScalarValue::parse(s));
    let map = |pairs: Vec<(&str, YamlValue)>| {
        YamlValue::Mapping(pairs.into_iter().map(|(k, v)| (k.to_string(), v)).collect())
    };

    assert_eq!(
        handler.deserialize("[1,2]"),
        Ok(YamlValue::Sequence(vec![num("1"), num("2")]))
    );
    assert_eq!(handler.deserialize("[]"), Ok(YamlValue::Sequence(vec![])));
    assert_eq!(handler.deserialize(" { } "), Ok(map(vec![])));
    assert_eq!(
        handler
            .deserialize(r#"{"a": [1, "x", true, null], "b":{"c":-1.5e2, "d\n\u00e9": "\"y\""}}"#),
        Ok(map(vec![
            (
                "a",
                YamlValue::Sequence(vec![
                    num("1"),
                    YamlValue::scalar("x"),
                    YamlValue::scalar(true),
                    YamlValue::Scalar(ScalarValue::null()),
                ])
            ),
            (
                "b",
                map(vec![
                    ("c", num("-1.5e2")),
                    ("d\n\u{e9}", YamlValue::scalar("\"y\"")),
                ])
            ),
        ]))
    );
    assert_eq!(
        handler.deserialize("{\r\n\t\"a\" : [\n  1 ,\n  [ ]\n ]\n}\n"),
        Ok(map(vec![(
            "a",
            YamlValue::Sequence(vec![num("1"), YamlValue::Sequence(vec![])])
        )]))
    );
    assert_eq!(handler.validate(r#"{"a": [{"b": []}]}"#), Ok(()));
}

#[test]
fn json_handler_rejects_yaml_only_syntax() {
    let handler = JsonHandler;
    for (content, message) in [
        ("{a: 1}", "Object keys must be strings"),
        ("{1: 2}", "Object keys must be strings"),
        (r#"{"a": x}"#, "Invalid JSON value"),
        ("['a']", "Invalid JSON value"),
        ("[1, , 2]", "Invalid JSON value"),
        (r#"{"a"}"#, "Invalid JSON value"),
        (r#"["a": 1]"#, "Invalid JSON value"),
        ("- 1", "Invalid JSON value"),
        ("a: 1", "Invalid JSON value"),
        ("--- [1]", "Invalid JSON value"),
        (
            "[1] [2]",
            "Invalid JSON: 1:5: Content after the document could not be parsed: \"[\"",
        ),
        ("[1, 2,]", "Trailing comma"),
        (r#"{"a": 1,}"#, "Trailing comma"),
        (r#"{"a": 1, "a": 2}"#, "Duplicate key"),
        ("[1, # c\n 2]", "Comments are not allowed in JSON"),
        ("[!t 1]", "Tags are not allowed in JSON"),
        ("!t [1]", "Tags are not allowed in JSON"),
        ("[&a 1]", "Anchors are not allowed in JSON"),
        ("[*a]", "Aliases are not allowed in JSON"),
        (r#"["a\x41"]"#, "Invalid escape sequence"),
        ("[\"a\n b\"]", "Unescaped control character in string"),
    ] {
        assert_eq!(
            handler.validate(content),
            Err(CustomTagError::with_content("!json", message, content)),
            "{}",
            content
        );
        // Lenient deserialization reads it as the raw string.
        assert_eq!(
            handler.deserialize(content),
            Ok(YamlValue::scalar(content)),
            "{}",
            content
        );
    }
}

#[test]
fn json_handler_reports_parse_errors() {
    let handler = JsonHandler;
    let err = handler.validate("[1, 2").unwrap_err();
    assert_eq!(
        err,
        CustomTagError::with_content(
            "!json",
            concat!(
                "Invalid JSON: 1:6: Unclosed flow sequence. Expected: ']' to close sequence. ",
                "Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close ",
                "the array, or check for missing commas between elements"
            ),
            "[1, 2"
        )
    );
    assert_eq!(handler.deserialize("[1, 2"), Ok(YamlValue::scalar("[1, 2")));
}

#[test]
fn json_handler_deserialize_returns_err_not_panic() {
    let handler = JsonHandler;
    let quote = handler.deserialize("\"").unwrap_err();
    assert_eq!(quote.tag, "!json");
    assert_eq!(quote.message, "Unterminated quoted string");
    assert_eq!(
        handler
            .deserialize("\"hello\"")
            .unwrap()
            .as_scalar()
            .unwrap()
            .value(),
        "hello"
    );
}

#[test]
fn json_handler_serialize_escapes_strings() {
    let handler = JsonHandler;
    assert_eq!(
        handler.serialize(&YamlValue::scalar("a\"b\\c")).unwrap(),
        r#""a\"b\\c""#
    );
    assert_eq!(
        handler
            .serialize(&YamlValue::scalar("l1\nl2\t\u{1}"))
            .unwrap(),
        r#""l1\nl2\t\u0001""#
    );
    assert_eq!(
        handler.serialize(&YamlValue::scalar("42")).unwrap(),
        "\"42\""
    );

    let mut map = std::collections::BTreeMap::new();
    map.insert("k\"".to_string(), YamlValue::scalar("v"));
    assert_eq!(
        handler.serialize(&YamlValue::Mapping(map)).unwrap(),
        r#"{"k\"":"v"}"#
    );
}

#[test]
fn json_handler_serialize_non_strings() {
    let handler = JsonHandler;
    assert_eq!(handler.serialize(&YamlValue::scalar(42i64)).unwrap(), "42");
    assert_eq!(handler.serialize(&YamlValue::scalar(-7i32)).unwrap(), "-7");
    assert_eq!(
        handler.serialize(&YamlValue::scalar(1.5f64)).unwrap(),
        "1.5"
    );
    assert_eq!(handler.serialize(&YamlValue::scalar(true)).unwrap(), "true");
    assert_eq!(
        handler
            .serialize(&YamlValue::Scalar(ScalarValue::null()))
            .unwrap(),
        "null"
    );
    assert_eq!(
        handler
            .serialize(&YamlValue::Scalar(ScalarValue::parse("0x1F")))
            .unwrap(),
        "31"
    );
    let err = handler
        .serialize(&YamlValue::scalar(f64::INFINITY))
        .unwrap_err();
    assert_eq!(err.message, "Value cannot be represented in JSON");
}

#[test]
fn json_handler_serialize_floats_as_written() {
    let handler = JsonHandler;
    let json = |text: &str| handler.serialize(&YamlValue::Scalar(ScalarValue::parse(text)));
    // JSON numbers have no range limit, so a float beyond f64 is kept.
    assert_eq!(json("1e400").unwrap(), "1e400");
    assert_eq!(json("-1.5E+400").unwrap(), "-1.5E+400");
    assert_eq!(json("1.0").unwrap(), "1.0");
    assert_eq!(
        json("0.10000000000000000000001").unwrap(),
        "0.10000000000000000000001"
    );
    // YAML spellings that JSON lacks.
    assert_eq!(json("+1.5").unwrap(), "1.5");
    assert_eq!(json(".5").unwrap(), "0.5");
    assert_eq!(json("-.5e3").unwrap(), "-0.5e3");
    assert_eq!(json("1.").unwrap(), "1.0");
    assert_eq!(json("1.e3").unwrap(), "1.0e3");
    assert_eq!(json("007.5").unwrap(), "7.5");
    assert_eq!(json("00.5").unwrap(), "0.5");
    assert_eq!(json("1e3").unwrap(), "1e3");

    for text in [".inf", "-.inf", "+.INF", ".nan", ".NaN"] {
        let err = json(text).unwrap_err();
        assert_eq!(err.message, "Value cannot be represented in JSON", "{text}");
    }
    let err = handler.serialize(&YamlValue::scalar(f64::NAN)).unwrap_err();
    assert_eq!(err.message, "Value cannot be represented in JSON");

    for text in ["1e400", "-2.5e-400", "1.0", "0.1"] {
        let value = handler.deserialize(text).unwrap();
        assert_eq!(handler.serialize(&value).unwrap(), text);
    }
}

#[test]
fn json_handler_serialize_integers_of_any_size() {
    let handler = JsonHandler;
    let json = |text: &str| {
        handler
            .serialize(&YamlValue::Scalar(ScalarValue::parse(text)))
            .unwrap()
    };
    assert_eq!(
        json("123456789012345678901234567890"),
        "123456789012345678901234567890"
    );
    assert_eq!(
        json("-123456789012345678901234567890"),
        "-123456789012345678901234567890"
    );
    assert_eq!(
        json("+00123456789012345678901234567890"),
        "123456789012345678901234567890"
    );
    assert_eq!(
        json("0x100000000000000000000000000000000"),
        "340282366920938463463374607431768211456"
    );
    assert_eq!(json("0o2000000000000000000000"), "18446744073709551616");

    // A JSON integer outside i64 comes back as it went in.
    let big = "-98765432109876543210987654321";
    let value = handler.deserialize(big).unwrap();
    assert_eq!(handler.serialize(&value).unwrap(), big);
}

#[test]
fn json_handler_deserialize_unescapes_strings() {
    let handler = JsonHandler;
    let value = |s: &str| handler.deserialize(s).unwrap().as_scalar().unwrap().clone();
    assert_eq!(value(r#""a\"b\u0041""#).value(), "a\"bA");
    assert_eq!(value(r#""\\\/\b\f\n\r\t""#).value(), "\\/\u{8}\u{c}\n\r\t");
    assert_eq!(value(r#""\ud83d\ude00""#).value(), "\u{1F600}");
    assert_eq!(
        value(r#""42""#).scalar_type(),
        yaml_edit::ScalarType::String
    );

    for bad in [
        r#""a"b""#,
        r#""a\x""#,
        r#""\u12""#,
        r#""\ud83d""#,
        "\"a\u{1}\"",
        r#""a\""#,
    ] {
        // Invalid JSON is read as the raw string, but does not validate.
        assert_eq!(value(bad).value(), &bad[1..bad.len() - 1], "{}", bad);
        let err = handler.validate(bad).unwrap_err();
        assert_eq!(err.tag, "!json", "{}", bad);
        assert_eq!(err.content.as_deref(), Some(bad), "{}", bad);
    }
}

#[test]
fn json_handler_deserialize_non_strings() {
    let handler = JsonHandler;
    let value = |s: &str| handler.deserialize(s).unwrap().as_scalar().unwrap().clone();
    assert_eq!(value("42").to_i64(), Some(42));
    assert_eq!(value("-0").to_i64(), Some(0));
    assert_eq!(value("1.5e3").to_f64(), Some(1500.0));
    assert_eq!(value("true").to_bool(), Some(true));
    assert_eq!(value("null").scalar_type(), yaml_edit::ScalarType::Null);

    for bad in ["plain", "0x1F", "01", "1.", ".5", "+1", "True", "~"] {
        assert_eq!(value(bad).value(), bad);
        let err = handler.validate(bad).unwrap_err();
        assert_eq!(err.message, "Invalid JSON value", "{}", bad);
    }
    assert_eq!(handler.validate("42"), Ok(()));
    assert_eq!(handler.validate(r#""a\"b""#), Ok(()));
}

#[test]
fn timestamp_handler() {
    let handler = TimestampHandler::new("YYYY-MM-DD");
    assert_eq!(
        handler.serialize(&YamlValue::scalar("2023-12-25")),
        Ok("2023-12-25".to_string())
    );
    assert_eq!(
        handler.deserialize("2023-12-25"),
        Ok(YamlValue::scalar(ScalarValue::timestamp("2023-12-25")))
    );
    assert_eq!(handler.validate("2023-12-25"), Ok(()));
    assert_eq!(
        handler.validate(""),
        Err(CustomTagError::with_content(
            "!timestamp",
            "Timestamp cannot be empty",
            ""
        ))
    );
}

#[test]
fn env_var_handler() {
    let handler = EnvVarHandler;
    std::env::set_var("TEST_VAR", "test_value");
    assert_eq!(
        handler.deserialize("TEST_VAR"),
        Ok(YamlValue::scalar("test_value"))
    );
    std::env::remove_var("TEST_VAR");

    assert_eq!(handler.validate("VALID_VAR_NAME"), Ok(()));
    assert_eq!(
        handler.validate(""),
        Err(CustomTagError::with_content(
            "!env",
            "Environment variable name cannot be empty",
            ""
        ))
    );
    assert_eq!(
        handler.validate("invalid var name"),
        Err(CustomTagError::with_content(
            "!env",
            "Invalid environment variable name",
            "invalid var name"
        ))
    );
}

#[test]
fn compressed_binary_handler() {
    let handler = CompressedBinaryHandler::new(6);
    // "Hello World" in base64.
    let base64 = "SGVsbG8gV29ybGQ=";
    assert_eq!(
        handler.serialize(&YamlValue::scalar(base64)),
        Ok("COMPRESSED[6]:SGVsbG8gV29ybGQ=".to_string())
    );
    assert_eq!(handler.deserialize(base64), Ok(YamlValue::scalar(base64)));
    assert_eq!(handler.validate(base64), Ok(()));
    assert_eq!(
        handler.validate("invalid@base64"),
        Err(CustomTagError::with_content(
            "!compressed",
            "Invalid base64 content",
            "invalid@base64"
        ))
    );
}

#[test]
fn registry_serialize_deserialize() {
    let mut registry = CustomTagRegistry::new();
    registry.register("!json", JsonHandler).unwrap();

    let scalar_value = YamlValue::scalar("test");
    assert_eq!(
        registry.serialize("!json", &scalar_value),
        Ok("\"test\"".to_string())
    );
    assert_eq!(
        registry.deserialize("!json", "\"hello\""),
        Ok(YamlValue::scalar("hello"))
    );

    let unregistered = Err(CustomTagError::new("!unknown", "Tag not registered"));
    assert_eq!(registry.serialize("!unknown", &scalar_value), unregistered);
    assert_eq!(
        registry.deserialize("!unknown", "content").map(|_| ()),
        unregistered.map(|_: String| ())
    );
}

/// Records the content of every node it is given and rejects "bad".
struct RecordingHandler(Arc<std::sync::Mutex<Vec<String>>>);

impl CustomTagHandler for RecordingHandler {
    fn serialize(&self, _value: &YamlValue) -> Result<String, CustomTagError> {
        Err(CustomTagError::new("!rec", "not used"))
    }

    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        self.0.lock().unwrap().push(content.to_string());
        if content == "bad" {
            return Err(CustomTagError::with_content("!rec", "rejected", content));
        }
        Ok(YamlValue::scalar(content))
    }

    fn description(&self) -> &str {
        "records its input"
    }
}

fn recording_parser() -> (CustomTagParser, Arc<std::sync::Mutex<Vec<String>>>) {
    let seen = Arc::new(std::sync::Mutex::new(Vec::new()));
    let mut registry = CustomTagRegistry::new();
    registry
        .register("!rec", RecordingHandler(Arc::clone(&seen)))
        .unwrap();
    (CustomTagParser::new(registry), seen)
}

/// A handler's default `validate` deserializes the content and reports
/// what that rejects.
#[test]
fn default_validate_deserializes() {
    let seen = Arc::new(std::sync::Mutex::new(Vec::new()));
    let mut registry = CustomTagRegistry::new();
    registry
        .register("!rec", RecordingHandler(Arc::clone(&seen)))
        .unwrap();
    assert_eq!(
        registry.validate("!rec", "bad"),
        Err(CustomTagError::with_content("!rec", "rejected", "bad"))
    );
    assert_eq!(registry.validate("!rec", "good"), Ok(()));
    assert_eq!(*seen.lock().unwrap(), vec!["bad", "good"]);
}

fn doc(text: &str) -> Document {
    Document::from_str(text).unwrap()
}

#[test]
fn parse_with_custom_tags_visits_every_tagged_node() {
    let (parser, seen) = recording_parser();
    let document = doc(concat!(
        "a: !rec one\n",
        "? !rec two\n",
        ": [!rec three, {!rec four: !rec '5'}]\n",
        "c: !rec {\"x\": !rec six}\n",
        "d:\n",
        "  - !rec \"seven\"\n",
    ));
    assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
    assert_eq!(
        *seen.lock().unwrap(),
        vec![
            "one",
            "two",
            "three",
            "four",
            "5",
            "{\"x\": !rec six}",
            "six",
            "seven"
        ]
    );
}

#[test]
fn parse_with_custom_tags_returns_first_error() {
    let (parser, seen) = recording_parser();
    let document = doc("[!rec ok, !rec bad, !rec bad]\n");
    assert_eq!(
        parser.parse_with_custom_tags(&document),
        Err(CustomTagError::with_content("!rec", "rejected", "bad"))
    );
    assert_eq!(*seen.lock().unwrap(), vec!["ok", "bad"]);
}

#[test]
fn parse_with_custom_tags_unregistered_tag() {
    let (parser, _) = recording_parser();
    let document = doc("a: !rec x\nb: !other y\n");
    assert_eq!(
        parser.parse_with_custom_tags(&document),
        Err(CustomTagError::new("!other", "Tag not registered"))
    );
}

#[test]
fn parse_with_custom_tags_block_collection() {
    // A handler sees a block collection as it would read at the top
    // level, without the indentation it has in the document.
    let (parser, seen) = recording_parser();
    let document = doc(concat!(
        "a: !rec\n",
        "  b: !rec x\n",
        "  c:\n",
        "    - 1\n",
        "d:\n",
        "  - !rec\n",
        "    -  y\n",
        "    - z\n",
    ));
    assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
    assert_eq!(
        *seen.lock().unwrap(),
        vec!["b: !rec x\nc:\n  - 1\n", "x", "-  y\n- z\n"]
    );

    let (parser, seen) = recording_parser();
    let document = doc("--- !rec\na: 1\nb:\n  - 2\n");
    assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
    assert_eq!(*seen.lock().unwrap(), vec!["a: 1\nb:\n  - 2\n"]);
}

#[test]
fn parse_with_custom_tags_block_collection_keeps_relative_indent() {
    // Blank lines, comments and block scalar bodies keep their
    // indentation relative to the collection.
    let (parser, seen) = recording_parser();
    let document = doc(concat!(
        "items:\n",
        "  - !rec\n",
        "    k: |\n",
        "      line\n",
        "        more\n",
        "\n",
        "    # note\n",
        "    j: \"a\n",
        "   b\"\n",
    ));
    assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
    assert_eq!(
        *seen.lock().unwrap(),
        vec!["k: |\n  line\n    more\n\n# note\nj: \"a\nb\"\n"]
    );
}

#[test]
fn process_tagged_block_mapping() {
    let (parser, _) = recording_parser();
    let document = doc("a: !rec\n  b: 1\n");
    let tagged = document.as_mapping().unwrap().get("a").unwrap();
    assert_eq!(
        parser.process_tagged_node(tagged.as_tagged().unwrap()),
        Ok(YamlValue::scalar("b: 1\n"))
    );
}

#[test]
fn process_tagged_json_object() {
    let mut registry = CustomTagRegistry::new();
    registry.register("!json", JsonHandler).unwrap();
    let parser = CustomTagParser::new(registry);
    let document = doc("data: !json {\"status\": \"active\"}\n");
    let mapping = document.as_mapping().unwrap();
    let data = mapping.get("data").unwrap();
    let tagged = data.as_tagged().unwrap();
    let mut expected = std::collections::BTreeMap::new();
    expected.insert("status".to_string(), YamlValue::scalar("active"));
    assert_eq!(
        parser.process_tagged_node(tagged),
        Ok(YamlValue::Mapping(expected))
    );
    assert_eq!(parser.parse_with_custom_tags(&document), Ok(()));
}

#[test]
fn parse_with_custom_tags_untagged_document() {
    let (parser, seen) = recording_parser();
    assert_eq!(
        parser.parse_with_custom_tags(&doc("a: [1, {b: c}]\n")),
        Ok(())
    );
    assert_eq!(*seen.lock().unwrap(), Vec::<String>::new());
}

#[test]
fn custom_tag_error() {
    let error = CustomTagError::new("!test", "Test error message");
    assert_eq!(error.tag, "!test");
    assert_eq!(error.message, "Test error message");
    assert!(error.content.is_none());

    let error_with_content = CustomTagError::with_content("!test", "Test error", "bad content");
    assert_eq!(error_with_content.content, Some("bad content".to_string()));

    assert_eq!(
        format!("{}", error_with_content),
        "Custom tag '!test' error: Test error (content: bad content)"
    );
}

#[test]
fn timestamp_validate_checks_format() {
    let handler = TimestampHandler::new("%Y-%m-%d");
    let expected = Err(CustomTagError::with_content(
        "!timestamp",
        "Expected format: %Y-%m-%d",
        "invalid-date",
    ));
    assert_eq!(handler.deserialize("invalid-date").map(|_| ()), expected);
    assert_eq!(handler.validate("invalid-date"), expected);
    assert_eq!(handler.validate("2024-01-15"), Ok(()));
}

#[test]
fn compressed_binary_validates_own_output() {
    let handler = CompressedBinaryHandler::new(6);
    let serialized = handler.serialize(&YamlValue::scalar("SGk=")).unwrap();
    assert_eq!(serialized, "COMPRESSED[6]:SGk=");
    assert_eq!(handler.validate(&serialized), Ok(()));
    assert_eq!(
        handler.validate("COMPRESSED[6]:no base64"),
        Err(CustomTagError::with_content(
            "!compressed",
            "Invalid base64 content",
            "COMPRESSED[6]:no base64",
        ))
    );
}

#[test]
fn registry_errors_name_registered_tag() {
    let mut registry = CustomTagRegistry::new();
    registry
        .register("!date", TimestampHandler::new("%Y-%m-%d"))
        .unwrap();
    let expected = CustomTagError::with_content("!date", "Timestamp cannot be empty", "");
    assert_eq!(registry.validate("!date", ""), Err(expected));
    assert_eq!(
        registry.deserialize("!date", "x"),
        Err(CustomTagError::with_content(
            "!date",
            "Expected format: %Y-%m-%d",
            "x"
        ))
    );
    assert_eq!(
        registry.serialize("!date", &YamlValue::sequence()),
        Err(CustomTagError::new("!date", "Value must be a scalar"))
    );
}

#[test]
fn json_handler_validate_messages() {
    let handler = JsonHandler;
    for (content, message) in [
        (r#"{"a" 1}"#, "Invalid JSON value"),
        (r#"{"a": 1, "b"}"#, "Invalid JSON value"),
        (r#"{"a": }"#, "Invalid JSON value"),
        (r#"{? "a": 1}"#, "Invalid JSON value"),
        (r#"["a": 1]"#, "Invalid JSON value"),
        ("{1: 2}", "Object keys must be strings"),
        ("{[1]: 2}", "Object keys must be strings"),
        ("[1,,2]", "Invalid JSON value"),
        ("[1 2]", "Invalid JSON value"),
        ("- 1", "Invalid JSON value"),
        ("a: 1", "Invalid JSON value"),
        ("--- 1", "Invalid JSON value"),
        ("1\n--- 2", "Invalid JSON value"),
        ("%YAML 1.2\n--- 1", "Invalid JSON value"),
        (r#"{"a":1}x"#, "Invalid JSON value"),
        ("", "Invalid JSON value"),
        ("-", "Invalid JSON value"),
        ("1e+", "Invalid JSON value"),
        (r#"{"a": 1, "a": 2}"#, "Duplicate key"),
        ("[1,]", "Trailing comma"),
        ("{,}", "Trailing comma"),
        ("[&x 1]", "Anchors are not allowed in JSON"),
        ("[*x]", "Aliases are not allowed in JSON"),
        ("[1] # c", "Comments are not allowed in JSON"),
        ("[!!int 1]", "Tags are not allowed in JSON"),
        (r#""\ud800A""#, "Unpaired surrogate in \\u escape"),
        (r#""\udc00""#, "Unpaired surrogate in \\u escape"),
        ("\"", "Unterminated quoted string"),
    ] {
        assert_eq!(
            handler.validate(content),
            Err(CustomTagError::with_content(
                "!json",
                message,
                content.trim()
            )),
            "{content}"
        );
    }
    assert_eq!(
        handler.deserialize("'"),
        Err(CustomTagError::with_content(
            "!json",
            "Unterminated quoted string",
            "'"
        ))
    );
}

#[test]
fn json_handler_nested_and_numbers() {
    let handler = JsonHandler;
    let value = handler
        .deserialize(r#"[1, [2.5e-3, {"a": [true, null], "b": "é"}]]"#)
        .unwrap();
    assert_eq!(handler.validate(r#"{"a":1,"b":2}"#), Ok(()));
    assert_eq!(
        handler.serialize(&value).unwrap(),
        r#"[1,[2.5e-3,{"a":[true,null],"b":"é"}]]"#
    );
    assert_eq!(handler.validate("1e5"), Ok(()));
    assert_eq!(handler.validate("-1.5E+3"), Ok(()));
}

#[test]
fn json_handler_serialize_escapes_and_unsupported() {
    let handler = JsonHandler;
    assert_eq!(
        handler.serialize(&YamlValue::scalar("a\r\u{8}\u{c}\u{1}\"\\\n\t")),
        Ok(r#""a\r\b\f\u0001\"\\\n\t""#.to_string())
    );
    assert_eq!(
        handler.serialize(&YamlValue::set()),
        Err(CustomTagError::new(
            "!json",
            "Unsupported value type for JSON serialization"
        ))
    );
    assert_eq!(handler.description(), "JSON data embedded in YAML");
}

#[test]
fn timestamp_handler_errors() {
    let handler = TimestampHandler::new("%Y-%m-%d");
    assert_eq!(
        handler.serialize(&YamlValue::scalar("2024-01-15")),
        Ok("2024-01-15".to_string())
    );
    assert_eq!(
        handler.serialize(&YamlValue::scalar("today")),
        Err(CustomTagError::with_content(
            "!timestamp",
            "Timestamp should contain year digits",
            "today"
        ))
    );
    assert_eq!(
        handler.serialize(&YamlValue::sequence()),
        Err(CustomTagError::new("!timestamp", "Value must be a scalar"))
    );
    assert_eq!(
        handler.deserialize("2024-01-15"),
        Ok(YamlValue::scalar(ScalarValue::timestamp("2024-01-15")))
    );
    assert_eq!(handler.description(), "Custom timestamp format handler");
    // A format without `%Y` does not check for digits.
    let handler = TimestampHandler::new("%H:%M");
    assert_eq!(
        handler.serialize(&YamlValue::scalar("noon")),
        Ok("noon".to_string())
    );
    assert_eq!(handler.validate("noon"), Ok(()));
}

#[test]
fn compressed_binary_handler_round_trip() {
    let handler = CompressedBinaryHandler::new(0);
    assert_eq!(
        handler.serialize(&YamlValue::scalar("SGk=")),
        Ok("SGk=".to_string())
    );
    assert_eq!(
        handler.serialize(&YamlValue::sequence()),
        Err(CustomTagError::new("!compressed", "Value must be a scalar"))
    );
    assert_eq!(
        handler.deserialize("COMPRESSED[3]:SGk="),
        Ok(YamlValue::scalar("SGk="))
    );
    assert_eq!(
        handler.deserialize("COMPRESSED[3"),
        Ok(YamlValue::scalar("COMPRESSED[3"))
    );
    assert_eq!(handler.deserialize("SGk="), Ok(YamlValue::scalar("SGk=")));
    assert_eq!(handler.validate("a+b/"), Ok(()));
    assert_eq!(handler.description(), "Compressed binary data");
}

#[test]
fn env_var_handler_errors() {
    let handler = EnvVarHandler;
    assert_eq!(
        handler.serialize(&YamlValue::scalar("HOME")),
        Ok("HOME".to_string())
    );
    assert_eq!(
        handler.serialize(&YamlValue::sequence()),
        Err(CustomTagError::new(
            "!env",
            "Environment variable must be a scalar"
        ))
    );
    assert_eq!(
        handler.deserialize("YAML_EDIT_SURELY_UNSET_VARIABLE"),
        Err(CustomTagError::with_content(
            "!env",
            "Environment variable not found",
            "YAML_EDIT_SURELY_UNSET_VARIABLE"
        ))
    );
    assert_eq!(handler.description(), "Environment variable substitution");
}

#[test]
fn registry_debug_clone_unregister() {
    let mut registry = CustomTagRegistry::new();
    registry.register("!json", JsonHandler).unwrap();
    registry.register("!env", EnvVarHandler).unwrap();
    assert_eq!(
        format!("{registry:?}"),
        r#"CustomTagRegistry { tags: ["!env", "!json"] }"#
    );
    // Clones share their handlers.
    let mut clone = registry.clone();
    assert!(clone.unregister("!env"));
    assert!(!clone.unregister("!env"));
    assert!(!registry.has_tag("!env"));
    assert_eq!(registry.registered_tags(), vec!["!json".to_string()]);
}

#[test]
fn custom_tag_error_display_without_content() {
    assert_eq!(
        CustomTagError::new("!t", "broken").to_string(),
        "Custom tag '!t' error: broken"
    );
}

#[test]
fn tag_name_with_invalid_named_handle() {
    let mut registry = CustomTagRegistry::new();
    for tag in ["timestamp", "!a_b!c", "!a.b!c", "!!", "!e!"] {
        assert_eq!(
            registry.register(tag, JsonHandler),
            Err(CustomTagError::new(
                tag,
                "Invalid tag name: must be a YAML tag such as '!name', '!!name' or '!<uri>'"
            )),
            "{tag}"
        );
    }
    assert_eq!(registry.register("!a-1!c", JsonHandler), Ok(()));
}

#[test]
fn json_handler_spaces_and_false() {
    let handler = JsonHandler;
    assert_eq!(
        handler.serialize(&YamlValue::scalar("a b")),
        Ok("\"a b\"".to_string())
    );
    assert_eq!(handler.validate("\"a b\""), Ok(()));
    assert_eq!(handler.validate("false"), Ok(()));
    assert_eq!(
        handler.deserialize("false"),
        Ok(YamlValue::Scalar(ScalarValue::from(false)))
    );
}

#[test]
fn verbatim_collection_tags() {
    let tagged = |yaml: &str| {
        YamlFile::from_str(yaml)
            .unwrap()
            .document()
            .unwrap()
            .as_mapping()
            .unwrap()
            .get("a")
            .unwrap()
            .as_tagged()
            .unwrap()
            .clone()
    };
    let set = tagged("a: !<tag:yaml.org,2002:set> {x, y}\n");
    let members: Vec<String> = set
        .as_set()
        .unwrap()
        .members()
        .map(|m| m.to_string())
        .collect();
    assert_eq!(members, vec!["x", "y"]);
    let omap = tagged("a: !<tag:yaml.org,2002:omap> [b: 1, c: 2]\n");
    assert_eq!(omap.as_ordered_mapping().map(|e| e.len()), Some(2));
    let pairs = tagged("a: !<tag:yaml.org,2002:pairs> [b: 1, b: 2]\n");
    assert_eq!(pairs.as_pairs().map(|e| e.len()), Some(2));
}

/// The tagged node under `key` in `yaml`.
fn tagged_value(yaml: &str, key: &str) -> TaggedNode {
    YamlFile::from_str(yaml)
        .unwrap()
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get(key)
        .unwrap()
        .as_tagged()
        .unwrap()
        .clone()
}

/// A tagged scalar's string value is unquoted; a tagged collection has
/// none, as its first item or key is not one.
#[test]
fn tagged_node_as_string() {
    for (yaml, key, expected) in [
        ("key: !custom hello", "key", Some("hello")),
        (r#"key: !custom "hello world""#, "key", Some("hello world")),
        ("a: !t [x, y]\nb: !t {\"q\": 1}\nc: !t\n  - x\n", "a", None),
        ("a: !t [x, y]\nb: !t {\"q\": 1}\nc: !t\n  - x\n", "b", None),
        ("a: !t [x, y]\nb: !t {\"q\": 1}\nc: !t\n  - x\n", "c", None),
    ] {
        assert_eq!(
            tagged_value(yaml, key).as_string(),
            expected.map(str::to_string),
            "{yaml:?} {key}"
        );
    }
}

/// `as_set` needs a mapping, and `as_ordered_mapping` and `as_pairs` a
/// sequence, each under its own tag.
#[test]
fn special_collection_accessors() {
    let members = |yaml: &str| {
        tagged_value(yaml, "key")
            .as_set()
            .map(|set| set.members().map(|m| m.to_string()).collect::<Vec<_>>())
    };
    assert_eq!(
        members("key: !!set {? a, ? b}\n"),
        Some(vec!["a".to_string(), "b".to_string()])
    );
    assert_eq!(members("key: !!set &s {}\n"), Some(vec![]));
    assert_eq!(members("key: !!set [a, b]\n"), None);
    assert_eq!(members("key: !!set\n- a\n"), None);
    assert_eq!(members("key: !!set\n"), None);

    let keys = |entries: Option<Vec<MappingEntry>>| {
        entries.map(|entries| {
            entries
                .iter()
                .map(|e| e.key_node().unwrap().to_string())
                .collect::<Vec<_>>()
        })
    };
    let ab = Some(vec!["a".to_string(), "b".to_string()]);
    let a = Some(vec!["a".to_string()]);
    assert_eq!(
        keys(tagged_value("key: !!omap [a: 1, b: 2]\n", "key").as_ordered_mapping()),
        ab
    );
    assert_eq!(
        keys(tagged_value("key: !!pairs [a: 1]\n", "key").as_pairs()),
        a
    );
    assert_eq!(
        keys(tagged_value("key: !!omap {a: 1}\n", "key").as_ordered_mapping()),
        None
    );
    assert_eq!(
        keys(tagged_value("key: !!pairs {a: 1}\n", "key").as_pairs()),
        None
    );
    assert_eq!(
        keys(tagged_value("key: !!omap\n  a: 1\n", "key").as_ordered_mapping()),
        None
    );

    let yaml = "o: !!omap [{a: 1}]\np: !!pairs [{a: 1}, {a: 2}]\n";
    assert_eq!(keys(tagged_value(yaml, "o").as_ordered_mapping()), a);
    assert_eq!(keys(tagged_value(yaml, "o").as_pairs()), None);
    assert_eq!(
        keys(tagged_value(yaml, "p").as_pairs()),
        Some(vec!["a".to_string(), "a".to_string()])
    );
    assert_eq!(keys(tagged_value(yaml, "p").as_ordered_mapping()), None);
}

/// The value of `key` in the `index`-th document of `src`.
fn document_value(src: &str, index: usize, key: &str) -> yaml_edit::YamlValue {
    let file = YamlFile::from_str(src).unwrap();
    let doc = file.documents().nth(index).unwrap();
    let value = yaml_edit::YamlValue::from_document(&doc).unwrap();
    value.as_mapping().unwrap()[key].clone()
}

#[test]
fn tag_handles_resolve_through_tag_directives() {
    let src = "%TAG !y! tag:yaml.org,2002:\n---\ni: !y!int 3\ns: !y!set {a, b}\n";
    let i = document_value(src, 0, "i");
    let i = i.as_scalar().unwrap();
    assert_eq!(i.scalar_type(), yaml_edit::ScalarType::Integer);
    // The tag is kept as written.
    assert_eq!(i.tag(), Some("!y!int"));
    assert!(document_value(src, 0, "s").is_set());
}

#[test]
fn redefined_secondary_handle_is_not_core() {
    let src = "%TAG !! tag:example.com,2000:\n---\na: !!int 12\n";
    let a = document_value(src, 0, "a");
    assert_eq!(
        a.as_scalar().unwrap().scalar_type(),
        yaml_edit::ScalarType::String
    );
}

#[test]
fn redefined_primary_handle() {
    let src = "%TAG ! tag:yaml.org,2002:\n---\na: !int 12\n";
    let a = document_value(src, 0, "a");
    assert_eq!(
        a.as_scalar().unwrap().scalar_type(),
        yaml_edit::ScalarType::Integer
    );
}

#[test]
fn tag_directives_apply_to_their_own_document() {
    let src = "%TAG !y! tag:yaml.org,2002:\n---\ni: !y!int 3\n---\ni: !y!int 3\n";
    let types: Vec<_> = (0..2)
        .map(|n| {
            document_value(src, n, "i")
                .as_scalar()
                .unwrap()
                .scalar_type()
        })
        .collect();
    assert_eq!(
        types,
        [
            yaml_edit::ScalarType::Integer,
            yaml_edit::ScalarType::String
        ]
    );
}

#[test]
fn resolved_tag() {
    let file = YamlFile::from_str(
        "%TAG !y! tag:yaml.org,2002:\n%TAG !e! tag:example.com,2000:\n---\n\
         - !y!int 1\n- !e!thing 2\n- !<tag:yaml.org,2002:str> 3\n- !local 4\n- !u!x 5\n",
    )
    .unwrap();
    let doc = file.document().unwrap();
    let seq = doc.as_sequence().unwrap();
    let tags: Vec<_> = seq
        .values()
        .map(|item| item.as_tagged().unwrap().resolved_tag().unwrap())
        .collect();
    assert_eq!(
        tags,
        [
            "!!int",
            "!<tag:example.com,2000:thing>",
            "!!str",
            "!local",
            "!u!x"
        ]
    );
}

#[test]
fn tags_compare_by_what_they_resolve_to() {
    let with_handle = YamlFile::from_str("%TAG !y! tag:yaml.org,2002:\n---\n!y!str 3\n").unwrap();
    let shorthand = Document::from_str("!!str 3\n").unwrap();
    assert!(yaml_edit::yaml_eq(
        &with_handle.document().unwrap(),
        &shorthand
    ));
}

#[test]
fn merge_key_tagged_through_a_handle() {
    let src = "%TAG !y! tag:yaml.org,2002:\n---\nx: &x {p: 1}\ny:\n  !y!merge <<: *x\n";
    let y = document_value(src, 0, "y");
    assert_eq!(y.as_mapping().unwrap().keys().collect::<Vec<_>>(), ["p"]);
}

#[test]
fn schema_types_tags_through_handles() {
    let file = YamlFile::from_str("%TAG !y! tag:yaml.org,2002:\n---\nk: !y!int 3\n").unwrap();
    let doc = file.document().unwrap();
    assert!(yaml_edit::SchemaValidator::failsafe()
        .strict()
        .validate(&doc)
        .is_err());
}

#[test]
fn omap_items_tagged_through_a_handle() {
    let src = "%TAG !y! tag:yaml.org,2002:\n---\no: !y!omap [!y!map {a: 1}, {b: 2}]\n";
    let o = document_value(src, 0, "o");
    assert!(o.is_ordered_mapping(), "{o:?}");
}
