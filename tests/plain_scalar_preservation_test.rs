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
fn colon_in_a_tag_suffix_stays_in_the_tag() {
    // `!!ss:eq` is one tag; the `:` only ends it where it would end a plain
    // scalar, i.e. followed by whitespace (`!!str: value`).
    let yaml = "tags: !!ss:eq\n- a\nnext: x\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(&file);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(keys, vec!["tags", "next"]);
    assert_eq!(
        mapping
            .get("tags")
            .unwrap()
            .as_tagged()
            .unwrap()
            .tag()
            .as_deref(),
        Some("!!ss:eq")
    );
}

#[test]
fn a_tag_before_a_colon_still_ends_at_the_colon() {
    // `!!str:` is where the colon *is* an indicator (whitespace follows), so
    // the tag ends there rather than swallowing it. yaml-edit keeps the tag
    // as a mapping key; PyYAML reads the trailing colon into the tag instead,
    // so only the tag boundary is asserted here.
    let yaml = "!!str: x\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    common::assert_file_cst_ok(&file);
    let tree = yaml_edit::debug::tree_to_string(file.syntax());
    assert!(!tree.contains("ERROR"), "{tree}");
    assert!(tree.contains("TAG: \"!!str\""), "{tree}");
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
