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
