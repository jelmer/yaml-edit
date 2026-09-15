mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{Mapping, YamlFile};

fn assert_metadata_edits(file: &YamlFile, mapping: &Mapping, yaml: &str) {
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(file);
    let metadata = mapping.get_mapping("metadata").unwrap();
    metadata.set("version", "0.1.1");
    common::assert_file_cst_ok(file);
    assert_eq!(
        file.to_string(),
        yaml.replace("version: 0.1.0", "version: 0.1.1")
    );
    metadata.set("status", "released");
    common::assert_file_cst_ok(file);
    assert_eq!(
        file.to_string(),
        yaml.replace("version: 0.1.0", "version: 0.1.1")
            .replace("status: draft", "status: released")
    );
}

fn assert_plain_description(value: &str) {
    let yaml =
        format!("description: {value}\nmetadata:\n  version: 0.1.0 # keep\n  status: draft\n");
    let file = YamlFile::from_str(&yaml).unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("description")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        value
    );
    assert_metadata_edits(&file, &mapping, &yaml);
}

#[test]
fn embedded_quotes_remain_plain_scalar_content() {
    for value in [
        "author's machine",
        "site's readiness",
        "uses \"quoted words\" here",
        "uses 'single quotes' here",
    ] {
        assert_plain_description(value);
    }
}

#[test]
fn block_text_keeps_commas_after_command_flags() {
    for value in [
        "follows sh -c, python3 -c and @resource files, and reports",
        "-c, python3",
        "--verbose, --log-level=debug",
    ] {
        assert_plain_description(value);
    }
}

#[test]
fn block_text_keeps_embedded_json_after_whitespace() {
    for value in [
        r#"Shape is {"entries":[{"name":"..."}]}."#,
        r#"call query-trends {"kind":"TrendsQuery"}"#,
        "text [one, two] follows",
    ] {
        assert_plain_description(value);
    }
}

#[test]
fn block_text_keeps_angle_brackets_before_commas() {
    assert_plain_description("Absolute path to a directory for the capture, the composition and the MP4. It is created if missing. Leave it out and it lands under ~/launch-video/<site>, never in the play run workspace, which is deleted when the run ends.");
}

#[test]
fn folded_scalar_indicator_and_flow_delimiters_stay_structural() {
    let yaml = "folded: >-\n  first line\n  second line\nflow: [~/launch-video/<site>, kept]\nmetadata:\n  version: 0.1.0\n  status: draft\n";
    let file = YamlFile::from_str(yaml).unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("folded")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "first line second line"
    );
    let flow = mapping.get_sequence("flow").unwrap();
    assert_eq!(flow.len(), 2);
    assert_eq!(
        flow.get(0).unwrap().as_scalar().unwrap().as_string(),
        "~/launch-video/<site>"
    );
    assert_eq!(
        flow.get(1).unwrap().as_scalar().unwrap().as_string(),
        "kept"
    );
    assert_metadata_edits(&file, &mapping, yaml);
}

#[test]
fn flow_sequence_keeps_double_dash_flags_as_scalars() {
    let yaml = "argv: [python3, '@resource{traps.py}', $project, --validate, -c, --probe]\nmetadata:\n  version: 0.1.0\n  status: draft\n";
    let file = YamlFile::from_str(yaml).unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let argv = mapping.get_sequence("argv").unwrap();
    let expected = [
        "python3",
        "@resource{traps.py}",
        "$project",
        "--validate",
        "-c",
        "--probe",
    ];
    assert_eq!(argv.len(), expected.len());
    for (index, expected) in expected.into_iter().enumerate() {
        assert_eq!(
            argv.get(index).unwrap().as_scalar().unwrap().as_string(),
            expected
        );
    }
    assert_metadata_edits(&file, &mapping, yaml);
}

#[test]
fn real_quotes_flow_delimiters_and_document_markers_stay_structural() {
    let yaml = "---\nquoted: 'author''s machine'\ndouble: \"quoted words\"\nflow: {list: [one, two], flag: --validate}\nblock:\n- --validate\n- ---not-a-marker\nmetadata:\n  version: 0.1.0\n  status: draft\n...\n---\nother: kept\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.documents().count(), 2);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("quoted")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "author's machine"
    );
    assert_eq!(
        mapping
            .get("double")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "quoted words"
    );
    let flow = mapping.get_mapping("flow").unwrap();
    assert_eq!(flow.len(), 2);
    let list = flow.get_sequence("list").unwrap();
    assert_eq!(list.len(), 2);
    assert_eq!(list.get(1).unwrap().as_scalar().unwrap().as_string(), "two");
    assert_eq!(
        flow.get("flag").unwrap().as_scalar().unwrap().as_string(),
        "--validate"
    );
    let block = mapping.get_sequence("block").unwrap();
    assert_eq!(block.len(), 2);
    assert_eq!(
        block.get(1).unwrap().as_scalar().unwrap().as_string(),
        "---not-a-marker"
    );
    assert_metadata_edits(&file, &mapping, yaml);
}

#[test]
fn a_tag_suffix_keeps_its_uri_characters() {
    // `ns-tag-char` is any URI character bar the flow indicators, so `:` and
    // `-` belong to the tag rather than ending it.
    for (yaml, tag) in [
        ("tags: !!ss:eq\n- a\nnext: x\n", "!!ss:eq"),
        ("tags: !!ss---\n- a\nnext: x\n", "!!ss---"),
        ("tags: !e:f\n- a\nnext: x\n", "!e:f"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["tags", "next"], "{yaml:?}");
        assert_eq!(
            mapping
                .get("tags")
                .unwrap()
                .as_tagged()
                .unwrap()
                .tag()
                .as_deref(),
            Some(tag),
            "{yaml:?}"
        );
    }
}

#[test]
fn a_flow_indicator_still_ends_a_tag() {
    let yaml = "a: [!t, x]\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let tree = yaml_edit::debug::tree_to_string(file.syntax());
    assert!(tree.contains("TAG: \"!t\""), "{tree}");
}

#[test]
fn a_colon_at_the_end_of_a_tag_belongs_to_the_tag() {
    // `:` is an ns-tag-char in YAML 1.2, so `!!str:` is the whole tag and
    // the value is what follows it. PyYAML reads it the same way.
    let yaml = "!!str: x\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(&file);
    let tree = yaml_edit::debug::tree_to_string(file.syntax());
    assert!(!tree.contains("ERROR"), "{tree}");
    assert!(tree.contains("TAG: \"!!str:\""), "{tree}");
}

#[test]
fn percent_inside_a_plain_scalar_is_not_a_directive() {
    // A directive is only a `%` that opens a line; elsewhere it is content.
    for (yaml, key, value) in [
        ("a: b%c\nnext: x\n", "a", "b%c"),
        ("a: 50%\nnext: x\n", "a", "50%"),
        ("k%j: v\nnext: x\n", "k%j", "v"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec![key.to_string(), "next".to_string()]);
        assert_eq!(
            mapping.get(key).unwrap().as_scalar().unwrap().as_string(),
            value
        );
    }
}

#[test]
fn directive_at_line_start_still_parses() {
    let yaml = "%YAML 1.2\n---\na: 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
        "1"
    );
}

#[test]
fn a_blank_line_continues_a_plain_scalar() {
    // A single line break folds to a space; a blank line keeps one newline.
    for (yaml, key, value) in [
        ("a: one\n  two\nb: y\n", "a", "one two"),
        ("a: one\n\n  two\nb: y\n", "a", "one\ntwo"),
        ("a: one\n\n\n  two\nb: y\n", "a", "one\n\ntwo"),
        ("a: one\n  two\n\n  three\nb: y\n", "a", "one two\nthree"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec![key.to_string(), "b".to_string()], "{yaml:?}");
        assert_eq!(
            mapping.get(key).unwrap().as_scalar().unwrap().as_string(),
            value,
            "{yaml:?}"
        );
    }
}

#[test]
fn a_document_level_plain_scalar_folds_unindented_lines() {
    // The document's own node has no enclosing collection to be confused
    // with, so a continuation need not be indented past the first line.
    for (yaml, value) in [
        ("ab\ncd\n", "ab cd"),
        ("ab\ncd\nef\n", "ab cd ef"),
        ("ab\n  cd\n", "ab cd"),
        ("ab\n\ncd\n", "ab\ncd"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
        assert_eq!(
            file.document().unwrap().as_scalar().unwrap().as_string(),
            value,
            "{yaml:?}"
        );
    }
}

#[test]
fn an_unindented_continuation_does_not_apply_inside_a_mapping() {
    // Indented past the key, this is a continuation; at the key's own
    // column it would be the next entry, so the rule stays strict there.
    let yaml = "a: one\n  two\nb: z\n";
    let file = YamlFile::from_str(yaml).unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["a", "b"]);
    assert_eq!(
        mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
        "one two"
    );
}

#[test]
fn a_sequence_entry_folds_a_continuation_at_its_own_column() {
    // A sequence entry's continuation only has to clear the sequence's
    // column, not the entry's content column, so `- x\n y` is one scalar.
    for yaml in [
        "a:\n- x\n y\nb: z\n",
        "a:\n- x\n  y\nb: z\n",
        "a:\n  - x\n   y\nb: z\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["a", "b"], "{yaml:?}");
        let seq = mapping.get("a").unwrap();
        let seq = seq.as_sequence().unwrap();
        assert_eq!(seq.len(), 1, "{yaml:?}");
        assert_eq!(
            seq.get(0).unwrap().as_scalar().unwrap().as_string(),
            "x y",
            "{yaml:?}"
        );
    }
}

#[test]
fn a_dash_line_still_starts_a_new_sequence_entry() {
    let yaml = "a:\n- x\n- y\nb: z\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let seq = mapping.get("a").unwrap();
    let seq = seq.as_sequence().unwrap();
    assert_eq!(seq.len(), 2);
}

#[test]
fn a_node_property_mid_scalar_is_plain_content() {
    // Per YAML 1.2 section 7.1 a tag, anchor or alias applies to the start
    // of a node. Once a plain scalar has begun, `!`, `&` and `*` are
    // ordinary characters, whether or not a space precedes them.
    for (yaml, value) in [
        ("a: x !!b\nnext: k\n", "x !!b"),
        ("a: x !t\nnext: k\n", "x !t"),
        ("a: x &anc\nnext: k\n", "x &anc"),
        ("a: x *ref\nnext: k\n", "x *ref"),
        ("a: x !t y\nnext: k\n", "x !t y"),
        // The shape a differential fuzz run reduced to: a tag-looking run
        // mid-scalar whose trailing `]` used to be left with nowhere to go.
        ("a: a:] !!b%as:]\nnext: k\n", "a:] !!b%as:]"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["a", "next"], "{yaml:?}");
        assert_eq!(
            mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
            value,
            "{yaml:?}"
        );
    }
}

#[test]
fn a_node_property_at_a_node_start_still_applies() {
    // The mid-scalar rule must not disturb properties in the positions
    // where they are real: after a colon, a dash, a document start, or
    // inside a flow collection.
    let yaml = "a: !!str x\nb: &anc y\nc: *anc\nd:\n- !!str z\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(&file);
    let tree = yaml_edit::debug::tree_to_string(file.syntax());
    assert!(!tree.contains("ERROR"), "{tree}");
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("a")
            .unwrap()
            .as_tagged()
            .unwrap()
            .tag()
            .as_deref(),
        Some("!!str")
    );
    assert_eq!(mapping.get("c").unwrap().as_alias().unwrap().name(), "anc");
    let seq = mapping.get("d").unwrap();
    let seq = seq.as_sequence().unwrap();
    assert_eq!(seq.len(), 1);

    // A document's own node, and a flow collection entry.
    let file = YamlFile::from_str("!!str v\n").unwrap();
    assert!(!yaml_edit::debug::tree_to_string(file.syntax()).contains("ERROR"));
    let file = YamlFile::from_str("a: [!!str x]\n").unwrap();
    assert!(!yaml_edit::debug::tree_to_string(file.syntax()).contains("ERROR"));
}

#[test]
fn a_comma_in_block_context_is_scalar_content() {
    // `,` separates entries only inside a flow collection. In block context
    // it is ordinary plain-scalar text, including at the start of a
    // continuation line, which used to strand the rest of the document.
    for (yaml, value) in [
        ("a: x,y\nb: z\n", "x,y"),
        ("a: x\n  ,y\nb: z\n", "x ,y"),
        ("a: ,y\nb: z\n", ",y"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["a", "b"], "{yaml:?}");
        assert_eq!(
            mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
            value,
            "{yaml:?}"
        );
    }
}

#[test]
fn a_comma_still_separates_flow_entries() {
    let file = YamlFile::from_str("a: [1, 2]\nb: {p: 1, q: 2}\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let seq = mapping.get("a").unwrap();
    assert_eq!(seq.as_sequence().unwrap().len(), 2);
    assert_eq!(mapping.get_mapping("b").unwrap().keys().count(), 2);
}

#[test]
fn a_verbatim_tag_keeps_its_uri() {
    // `!<uri>` runs to its closing `>` and may hold any URI character,
    // commas included.
    let yaml = "!<tag:yaml.org,2002:str> foo\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(&file);
    let tree = yaml_edit::debug::tree_to_string(file.syntax());
    assert!(!tree.contains("ERROR"), "{tree}");
    assert!(tree.contains("TAG: \"!<tag:yaml.org,2002:str>\""), "{tree}");
}

/// A `-` not followed by a space is scalar content, not a sequence
/// indicator, so `-{ [a]: v }` is a mapping whose key is the plain scalar
/// `-{ [a]` -- as both saphyr and PyYAML read it.
///
/// The `-` arm's body reader stopped at the first space, so the `[` after
/// it lexed as a flow collection instead of scalar content. The rest of the
/// line then had nowhere to go and was stranded in an ERROR node with no
/// parse error reported.
#[test]
fn test_dash_prefixed_key_keeps_flow_indicator_as_content() {
    for yaml in [
        "-{ [a]: v }\n",
        ".{ [a]: v }\n",
        "-{ [al b]: value1  value2 }\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        assert!(
            !yaml_edit::debug::tree_to_string(file.syntax()).contains("ERROR"),
            "stranded tokens for {yaml:?}"
        );
    }

    let file = YamlFile::from_str("-{ [a]: v }\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .iter()
        .map(|(k, _)| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["-{ [a]".to_string()]);
}

/// A plain scalar starting with `-` or `.` spans internal spaces like any
/// other, rather than ending at the first one.
#[test]
fn test_dash_prefixed_scalar_spans_internal_spaces() {
    let file = YamlFile::from_str("args: --verbose --log-level=debug\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("args")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "--verbose --log-level=debug"
    );
}

/// A `-` closing a block-scalar header is a chomping indicator and stands
/// alone, so it must not absorb the space after it.
#[test]
fn test_block_scalar_chomping_dash_is_not_scalar_content() {
    let yaml = "key: >-\n  folded body\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    assert!(!yaml_edit::debug::tree_to_string(file.syntax()).contains("ERROR"));
}

/// `?` is an explicit-key indicator only at the start of a node. Once a
/// plain scalar has begun it is ordinary content, so `a ?,b` is one scalar.
/// Lexing it as QUESTION left the following `,b` with nowhere to go and
/// stranded it in an ERROR node with no parse error.
#[test]
fn test_question_mark_mid_scalar_is_plain_content() {
    for yaml in ["a ?,b\n", "a ?b\n", ")  .exp  ?,expect(\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        assert!(
            !yaml_edit::debug::tree_to_string(file.syntax()).contains("ERROR"),
            "stranded tokens for {yaml:?}"
        );
    }

    let file = YamlFile::from_str("x ?y: v\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .iter()
        .map(|(k, _)| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["x ?y".to_string()]);
}

/// A `?` that really does start a node still opens an explicit key.
#[test]
fn test_question_mark_at_node_start_still_opens_explicit_key() {
    for yaml in [
        "? key\n: value\n",
        "keys: !!set\n  ? a\n  ? b\n",
        "map:\n  ? complex\n  : v\n",
        "a: {? k: v}\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        assert!(
            !yaml_edit::debug::tree_to_string(file.syntax()).contains("ERROR"),
            "stranded tokens for {yaml:?}"
        );
    }

    let file = YamlFile::from_str("? key\n: value\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
}

/// Outside a flow collection the flow indicators are ordinary plain-scalar
/// content, so a `+` followed by one continues the scalar. The `+` arm only
/// let a non-special character begin a body, so `a+[b]` emitted a bare PLUS
/// and let `[` open a flow sequence, stranding the rest with no parse error.
///
/// A regex is the realistic shape: `[\.a-zA-Z]{2,}$` after a `+` quantifier.
#[test]
fn test_plus_before_flow_indicator_stays_scalar_content() {
    for yaml in ["a+[b]\n", "a+{b}\n", "a: x+[1]\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }

    let pattern = r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+[\.a-zA-Z]{2,}$";
    let yaml = format!("pattern: {pattern}\nnext: keep\n");
    let file = YamlFile::from_str(&yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("pattern")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        pattern
    );
    assert_eq!(
        mapping
            .get("next")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "keep"
    );
}

/// A `+` sign prefix and a block-scalar chomping `+` keep their meanings.
#[test]
fn test_plus_keeps_sign_and_chomping_roles() {
    for (yaml, value) in [("v: +5\n", "+5"), ("f: +.INF\n", "+.INF")] {
        let file = YamlFile::from_str(yaml).unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        assert_eq!(
            mapping
                .get(yaml.split(':').next().unwrap())
                .unwrap()
                .as_scalar()
                .unwrap()
                .as_string(),
            value
        );
    }

    let yaml = "b: |+\n  x\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(&file);
}

/// A `:` that is not followed by whitespace is scalar content, so it can
/// begin a plain-scalar body after a `+`. The `+` arm let only a
/// non-special character start one, so `a {b+:c}` emitted a bare PLUS and
/// the trailing `}` was stranded in an ERROR node with no parse error.
#[test]
fn test_plus_before_non_indicator_colon_stays_scalar_content() {
    for yaml in [
        "a {b+:c}\n",
        "a +:b}\n",
        "call query-trends {kind+:\"TrendsQuery\"}\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }
}

/// A `:` that really is a mapping indicator still ends the key.
#[test]
fn test_plus_key_still_ends_at_a_mapping_colon() {
    let yaml = "a: b\n+c: d\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["a".to_string(), "+c".to_string()]);
}

/// A bare `+` before a mapping colon is the key, exactly as a bare `-` is.
/// The `+` arm emitted a PLUS token there, so the mapping exposed no key at
/// all and `: v` was stranded in an ERROR node with no parse error.
#[test]
fn test_bare_plus_is_a_mapping_key() {
    let yaml = "+: v\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(&file);
    let tree = yaml_edit::debug::tree_to_string(file.syntax());
    assert!(!tree.contains("ERROR"), "{tree}");

    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["+".to_string()]);
    assert_eq!(
        mapping.get("+").unwrap().as_scalar().unwrap().as_string(),
        "v"
    );

    // The `-` spelling this now matches.
    let file = YamlFile::from_str("-: v\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.get("-").unwrap().as_scalar().unwrap().as_string(),
        "v"
    );
}

/// `|` and `>` open a block scalar only at the start of a node. Inside a
/// plain scalar they are ordinary content, so `a|b: v` is keyed `a|b`, as
/// both saphyr and PyYAML read it. Lexing the `|` as a header split the key
/// and stranded the following entry in an ERROR node with no parse error.
#[test]
fn test_block_scalar_indicator_mid_scalar_is_plain_content() {
    for (yaml, key) in [
        ("a|b: v\nc: d\n", "a|b"),
        ("a>b: v\nc: d\n", "a>b"),
        ("a+b: v\nc: d\n", "a+b"),
        ("a|: v\nc: d\n", "a|"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");

        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec![key.to_string(), "c".to_string()], "{yaml:?}");
    }
}

/// A `|` or `>` that does start a node still opens a block scalar, and the
/// header keeps its explicit indent and chomping indicators.
#[test]
fn test_block_scalar_headers_still_lex() {
    for yaml in [
        "k: |\n  body\n",
        "k: >\n  folded\n",
        "k: |-\n  x\n",
        "k: |+\n  x\n",
        "k: |2\n   x\n",
        "s:\n  - |\n    b\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }
}

/// A `!` on a continuation line is scalar content, not a tag. The lexer
/// reads it as a TAG because a newline precedes it, but a node property
/// applies only at the start of a node and a continuation line is already
/// inside one. The continuation check did not count a TAG as content, so
/// the scalar ended and the tag was stranded in an ERROR node with no
/// parse error.
#[test]
fn test_tag_on_a_continuation_line_is_scalar_content() {
    for yaml in ["- a\n !\n", "- a\n !x\n", "x\n !\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }

    // Both lines fold into one scalar, as PyYAML and saphyr read them.
    let file = YamlFile::from_str("- a\n !\n").unwrap();
    let seq = file.document().unwrap().as_sequence().unwrap();
    assert_eq!(seq.len(), 1);
    assert_eq!(seq.get(0).unwrap().as_scalar().unwrap().as_string(), "a !");
}

/// A tag that really does start a node is still a tag.
#[test]
fn test_tag_at_a_node_start_still_applies() {
    let file = YamlFile::from_str("a: !!str v\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("a")
            .unwrap()
            .as_tagged()
            .unwrap()
            .tag()
            .as_deref(),
        Some("!!str")
    );
}

/// `?` opens an explicit key only when a space or line break follows it.
/// Glued to what comes next it is ordinary plain-scalar content, which the
/// YAML test suite states directly: 652Z expects `=VAL :?foo`, and 2EBW
/// and FBC9 both list `?foo` as a safe plain scalar.
///
/// Without that test `a: ?!!r{2}x` opened an explicit key and then read
/// `!!r` as a tag, so the `{2}` closed a flow collection that was never
/// opened and the rest of the line was stranded in an ERROR node with no
/// parse error.
#[test]
fn test_question_mark_without_a_space_is_scalar_content() {
    for yaml in [
        "a: ?!!r{2}x\n",
        "a: ?x\n",
        "?key: v\n",
        "es_ttpattern: ?!!r\\d{2}-\\d{2}'\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }

    // The regex keeps every character, as PyYAML reads it.
    let yaml = "es_ttpattern: ?!!r\\d{2}-\\d{2}'\n";
    let file = YamlFile::from_str(yaml).unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("es_ttpattern")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "?!!r\\d{2}-\\d{2}'"
    );

    // A `?` glued to a key keeps it in the key.
    let file = YamlFile::from_str("?key: v\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["?key".to_string()]);
}

/// A `?` followed by a space still opens an explicit key, in block and flow.
#[test]
fn test_question_mark_with_a_space_still_opens_an_explicit_key() {
    for yaml in [
        "? key\n: value\n",
        "keys: !!set\n  ? a\n  ? b\n",
        "map:\n  ? complex\n  : v\n",
        "a: {? k: v}\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }

    let file = YamlFile::from_str("? key\n: value\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.get("key").unwrap().as_scalar().unwrap().as_string(),
        "value"
    );
}

/// A quote no longer opens a quoted scalar once a plain one has begun, so
/// `+'a': 1` is keyed `+'a'`, as PyYAML reads it. The `+` arm did not count
/// a quote as starting a scalar body, so it emitted a bare PLUS and the
/// quoted run became a scalar of its own, stranding the `: 1` after it.
#[test]
fn test_plus_before_a_quote_stays_scalar_content() {
    for (yaml, key) in [
        ("+'a': 1\n", "+'a'"),
        ("+\"a\": 1\n", "+\"a\""),
        ("+'a' : 1\n", "+'a'"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");

        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec![key.to_string()], "{yaml:?}");
    }
}

/// A quote that really does start a node still opens a quoted scalar.
#[test]
fn test_quote_at_a_node_start_still_quotes() {
    let file = YamlFile::from_str("'quoted': v\na: 'q'\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("quoted")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "v"
    );
    assert_eq!(
        mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
        "q"
    );
}

/// A `+` or `-` is an indicator only where a node starts, so once a plain
/// scalar has begun a second sign is content: `++: v` is keyed `++`, as
/// both saphyr and PyYAML read it.
///
/// The `+` arm did not count a sign as starting a scalar body, so `++`
/// lexed as a bare PLUS followed by a separate `+`, and the colon after it
/// was stranded in an ERROR node with no parse error.
#[test]
fn test_repeated_sign_is_one_plain_scalar() {
    for (yaml, key) in [("++: v\n", "++"), ("+-: v\n", "+-"), ("--: v\n", "--")] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");

        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec![key.to_string()], "{yaml:?}");
    }

    // The same in a value position.
    let file = YamlFile::from_str("a: ++\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
        "++"
    );
}

/// A single sign still prefixes a number, and a block-scalar chomping `+`
/// keeps its own token.
#[test]
fn test_single_sign_keeps_its_meaning() {
    for (yaml, value) in [
        ("v: +5\n", "+5"),
        ("v: -5\n", "-5"),
        ("v: +.INF\n", "+.INF"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        assert_eq!(
            mapping.get("v").unwrap().as_scalar().unwrap().as_string(),
            value,
            "{yaml:?}"
        );
    }

    let yaml = "k: |+2\n  x\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(&file);
}

/// A scalar that starts with `:` keeps flow indicators as content outside a
/// flow collection, like any other plain scalar. `- :m{` is the single item
/// `:m{`, as both saphyr and PyYAML read it.
///
/// The `:` arm broke at every YAML-special character, so the `{` ended the
/// scalar and was stranded in an ERROR node with no parse error, while the
/// identically shaped `- x{` lexed as one token.
#[test]
fn test_colon_prefixed_scalar_keeps_flow_indicators() {
    for (yaml, value) in [
        ("- :m{\n", ":m{"),
        ("- :m}x\n", ":m}x"),
        ("- :m,n\n", ":m,n"),
        ("- :m[1]\n", ":m[1]"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");

        let seq = file.document().unwrap().as_sequence().unwrap();
        assert_eq!(seq.len(), 1, "{yaml:?}");
        assert_eq!(
            seq.get(0).unwrap().as_scalar().unwrap().as_string(),
            value,
            "{yaml:?}"
        );
    }

    // The same in a value position.
    let file = YamlFile::from_str("a: :m{\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.get("a").unwrap().as_scalar().unwrap().as_string(),
        ":m{"
    );
}

/// Inside a flow collection those indicators still delimit, so a
/// colon-prefixed scalar ends at a comma or bracket.
#[test]
fn test_colon_prefixed_scalar_still_ends_in_flow_context() {
    let yaml = "a: [::v, ::w]\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(&file);

    let mapping = file.document().unwrap().as_mapping().unwrap();
    let seq = mapping.get("a").unwrap();
    let seq = seq.as_sequence().unwrap();
    assert_eq!(seq.len(), 2);
    assert_eq!(seq.get(0).unwrap().as_scalar().unwrap().as_string(), "::v");
    assert_eq!(seq.get(1).unwrap().as_scalar().unwrap().as_string(), "::w");
}

/// An anchor or alias name runs to whitespace or a flow indicator, as
/// `ns-anchor-name` has it: `-`, `*` and `:` are ordinary name characters.
/// saphyr reads `&xT*U---` as the single anchor `xT*U---`.
///
/// The name was read with the general scalar reader, which stops at every
/// YAML-special character, so the rest of the name was stranded in an ERROR
/// node with no parse error.
#[test]
fn test_anchor_name_runs_to_whitespace_or_flow_indicator() {
    for (yaml, name) in [
        ("&xT*U---", "&xT*U---"),
        ("&a---\n", "&a---"),
        ("&a:b\n", "&a:b"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
        assert!(
            tree.contains(&format!("ANCHOR: {name:?}")),
            "{yaml:?}\n{tree}"
        );
    }

    // A flow indicator still ends the name.
    let file = YamlFile::from_str("[&a x, *a]\n").unwrap();
    let tree = yaml_edit::debug::tree_to_string(file.syntax());
    assert!(tree.contains("ANCHOR: \"&a\""), "{tree}");
    assert!(tree.contains("REFERENCE: \"*a\""), "{tree}");
}

/// Anchors and aliases still resolve normally, including merge keys.
#[test]
fn test_anchor_name_change_keeps_resolution() {
    for yaml in [
        "a: &anc 1\nb: *anc\n",
        "defaults: &d\n  t: 30\nprod:\n  <<: *d\n  h: p\n",
        "- &x 1\n- *x\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }
}

/// An anchor on a continuation line is scalar content, not a node property.
///
/// `k:#foo\n &a !t s\n` is the single scalar `k:#foo &a !t s` (no space
/// before the `#`, so it is not a comment), as both saphyr and PyYAML read
/// it. The continuation check did not count an ANCHOR as content, so the
/// scalar ended and the rest was stranded in an ERROR node with no parse
/// error (suite case 3MYT).
#[test]
fn test_anchor_on_a_continuation_line_is_scalar_content() {
    let yaml = "k:#foo\n &a !t s\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let tree = yaml_edit::debug::tree_to_string(file.syntax());
    assert!(!tree.contains("ERROR"), "{tree}");
    assert_eq!(
        file.document().unwrap().as_scalar().unwrap().as_string(),
        "k:#foo &a !t s"
    );

    // An anchor that really starts a node still annotates it.
    for yaml in ["a: &anc 1\nb: *anc\n", "- &x 1\n- *x\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        common::assert_file_cst_ok(&file);
        let tree = yaml_edit::debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml:?}\n{tree}");
    }
}
