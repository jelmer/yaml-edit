mod common;

use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, YamlFile};

#[test]
fn anchored_indentless_sequence_preserves_sibling_mapping_and_edits() {
    for indent in ["", "  "] {
        for comment in ["", " # shared tags"] {
            let yaml = format!(
            "tags: &id001{comment}\n{indent}- engineering\nmetadata:\n  version: 0.1.0 # keep version comment\n  status: draft\n  tags: *id001\n"
        );
            let file = YamlFile::from_str(&yaml).unwrap();
            assert_eq!(file.to_string(), yaml);
            let doc = file.document().unwrap();
            let mapping = doc.as_mapping().unwrap();
            assert_eq!(
                mapping.keys().count(),
                2,
                "{}",
                debug::tree_to_string(file.syntax())
            );
            let tags = mapping.get("tags").unwrap();
            let tags = tags.as_sequence().unwrap();
            assert_eq!(tags.len(), 1);
            assert_eq!(
                tags.get(0).unwrap().as_scalar().unwrap().as_string(),
                "engineering"
            );
            let metadata = mapping.get_mapping("metadata").unwrap();
            assert_eq!(
                metadata.get("tags").unwrap().as_alias().unwrap().name(),
                "id001"
            );
            metadata.set("version", "0.1.1");
            common::assert_file_cst_ok(&file);
            assert_eq!(
                file.to_string(),
                yaml.replace("version: 0.1.0", "version: 0.1.1")
            );
            metadata.set("status", "stable");
            common::assert_file_cst_ok(&file);
            assert_eq!(
                file.to_string(),
                yaml.replace("version: 0.1.0", "version: 0.1.1")
                    .replace("status: draft", "status: stable")
            );
        }
    }
}

#[test]
fn tagged_block_node_survives_blank_and_comment_lines() {
    // Blank and comment-only lines between a tag and the indented node it
    // annotates belong to neither, so they must not detach the two. The tag
    // used to end up holding an implicit null, with the sequence and every
    // later sibling entry landing in an ERROR node.
    for separator in ["\n", "# c\n", "\n# c\n\n"] {
        let yaml = format!("k: !!seq\n{separator}  - a\nb: 1\n");
        let file = YamlFile::from_str(&yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        assert_eq!(
            mapping.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
            vec!["k", "b"],
            "{}",
            debug::tree_to_string(file.syntax())
        );
        assert_eq!(
            mapping.get("k").unwrap().as_tagged().unwrap().tag(),
            Some("!!seq".to_string())
        );
    }
}

#[test]
fn tagged_mapping_survives_a_blank_line() {
    // Same shape with a mapping. This one parsed without an ERROR node but
    // leaked `a: 1` out to the top level, reporting three keys where PyYAML
    // reports two.
    let yaml = "k: !!map\n\n  a: 1\nb: 2\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
        vec!["k", "b"],
        "{}",
        debug::tree_to_string(file.syntax())
    );
    assert_eq!(
        mapping.get("k").unwrap().as_tagged().unwrap().tag(),
        Some("!!map".to_string())
    );
}

#[test]
fn tag_does_not_adopt_a_following_sibling_entry() {
    // `b: 1` is a sibling of `k`, not the value the tag annotates: it is not
    // indented past the key, so skipping the blank line must not reach it.
    let yaml = "k: !!str\n\nb: 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
        vec!["k", "b"],
        "{}",
        debug::tree_to_string(file.syntax())
    );
}

#[test]
fn tag_alone_at_end_of_input_is_an_implicit_null() {
    // Nothing follows the blank or comment line, so there is no node to
    // attach; the scan must stop at end of input rather than run off it.
    for yaml in ["k: !!seq\n", "k: !!seq\n\n", "k: !!seq\n# c\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        assert_eq!(
            mapping.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
            vec!["k"],
            "{}",
            debug::tree_to_string(file.syntax())
        );
    }
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
        assert_eq!(root.keys().count(), 3);
        let merged = root.get_mapping("y").unwrap();
        assert_eq!(merged.keys().count(), 2);
        let merge_key = merged.keys().next().unwrap();
        assert_eq!(merge_key.as_scalar().unwrap().as_string(), "<<");
        assert_eq!(
            merged.get(&merge_key).unwrap().as_alias().unwrap().name(),
            "defaults"
        );
        assert_eq!(
            merged.get("more").unwrap().as_scalar().unwrap().as_string(),
            "kept"
        );
        let metadata = root.get_mapping("metadata").unwrap();
        metadata.set("version", "0.1.1");
        common::assert_file_cst_ok(&file);
        assert_eq!(
            file.to_string(),
            yaml.replace("version: 0.1.0", "version: 0.1.1")
        );
        metadata.set("status", "stable");
        common::assert_file_cst_ok(&file);
        assert_eq!(
            file.to_string(),
            yaml.replace("version: 0.1.0", "version: 0.1.1")
                .replace("status: draft", "status: stable")
        );
    }
}
