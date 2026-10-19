//! Where an entry added at the end of a collection goes when the
//! collection ends in blank lines.

use std::str::FromStr;
use yaml_edit::{Mapping, Sequence, YamlFile};

mod common;
use common::assert_file_parser_shaped;

fn edit(source: &str, f: impl FnOnce(&YamlFile)) -> String {
    let file = YamlFile::from_str(source).unwrap();
    f(&file);
    assert_file_parser_shaped(&file);
    file.to_string()
}

fn root_mapping(file: &YamlFile) -> Mapping {
    file.document().unwrap().as_mapping().unwrap()
}

fn root_sequence(file: &YamlFile) -> Sequence {
    file.document().unwrap().as_sequence().unwrap()
}

#[test]
fn nested_collection_keeps_blank_lines_at_its_end() {
    assert_eq!(
        edit("a:\n  x: 1\n\nb: 2\n", |f| root_mapping(f)
            .get_mapping("a")
            .unwrap()
            .set("y", 3)),
        "a:\n  x: 1\n  y: 3\n\nb: 2\n"
    );
    assert_eq!(
        edit("a:\n  - x\n\nb: 2\n", |f| root_mapping(f)
            .get_sequence("a")
            .unwrap()
            .push("y")),
        "a:\n  - x\n  - y\n\nb: 2\n"
    );
    assert_eq!(
        edit("a:\n  x: 1\n  # c\n\n", |f| root_mapping(f)
            .get_mapping("a")
            .unwrap()
            .set("y", 3)),
        "a:\n  x: 1\n  y: 3\n  # c\n\n"
    );
}

#[test]
fn root_collection_appends_after_blank_lines() {
    assert_eq!(
        edit("x: 1\n\n", |f| root_mapping(f).set("y", 2)),
        "x: 1\n\ny: 2\n"
    );
    assert_eq!(
        edit("- x\n\n", |f| root_sequence(f).push("y")),
        "- x\n\n- y\n"
    );
    assert_eq!(
        edit("- x\n\n", |f| root_sequence(f).insert(1, "y")),
        "- x\n\n- y\n"
    );
    assert_eq!(
        edit("- x\n\n# c\n\n", |f| root_sequence(f).push("y")),
        "- x\n\n# c\n\n- y\n"
    );
}
