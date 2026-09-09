mod common;

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
