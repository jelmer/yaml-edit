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
