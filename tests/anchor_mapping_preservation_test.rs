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
fn tagged_indentless_sequence_preserves_sibling_mapping_and_edits() {
    for tag in ["!!seq", "!keep"] {
        for indent in ["", "  "] {
            for comment in ["", " # shared tags"] {
                let yaml = format!(
                    "tags: {tag}{comment}\n{indent}- engineering\nmetadata:\n  version: 0.1.0 # keep version comment\n  status: draft\n"
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
                // The tag stays reachable through TaggedNode regardless of
                // whether the sequence it annotates is indented.
                let tags = mapping.get("tags").unwrap();
                assert_eq!(
                    tags.as_tagged().unwrap().tag().as_deref(),
                    Some(tag),
                    "{}",
                    debug::tree_to_string(file.syntax())
                );
                let metadata = mapping.get_mapping("metadata").unwrap();
                metadata.set("version", "0.1.1");
                common::assert_file_cst_ok(&file);
                assert_eq!(
                    file.to_string(),
                    yaml.replace("version: 0.1.0", "version: 0.1.1")
                );
            }
        }
    }
}

#[test]
fn tagged_indentless_sequence_value_copies_with_its_tag() {
    let yaml = "tags: !!seq\n- engineering\nb: 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let tags = mapping.get("tags").unwrap();
    mapping.set("b", tags);
    common::assert_file_cst_ok(&file);
    assert_eq!(
        mapping
            .get("b")
            .unwrap()
            .as_tagged()
            .unwrap()
            .tag()
            .as_deref(),
        Some("!!seq")
    );
}

#[test]
fn tagged_indentless_sequence_nested_in_a_mapping() {
    // The `-` sits at the same column as its key, which is itself indented.
    let yaml = "outer:\n  tags: !!seq\n  - engineering\n  b: 1\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let outer = file
        .document()
        .unwrap()
        .as_mapping()
        .unwrap()
        .get_mapping("outer")
        .unwrap();
    assert_eq!(
        outer.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
        vec!["tags", "b"],
        "{}",
        debug::tree_to_string(file.syntax())
    );
    assert_eq!(
        outer.get("tags").unwrap().as_tagged().unwrap().tag(),
        Some("!!seq".to_string())
    );
}

#[test]
fn tagged_sequence_dedented_past_its_key_is_not_absorbed() {
    // `- a` at column 0 cannot belong to `tags`, which is indented by two.
    // PyYAML rejects this outright; the CST must not quietly adopt it into
    // the tagged node.
    let yaml = "outer:\n  tags: !!seq\n- a\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let tree = debug::tree_to_string(file.syntax());
    assert!(tree.contains("ERROR"), "{tree}");
}

#[test]
fn tagged_inline_scalar_still_parses() {
    let yaml = "count: !!int 42\nname: keep\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.keys().count(), 2);
    assert_eq!(
        mapping
            .get("count")
            .unwrap()
            .as_tagged()
            .unwrap()
            .tag()
            .as_deref(),
        Some("!!int")
    );
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

#[test]
fn indentless_tagged_set_does_not_absorb_following_entries() {
    // An indentless block mapping cannot be a mapping value, so the explicit
    // keys and the sibling entry all dedent out to the enclosing mapping and
    // the tag is left annotating an implicit null. PyYAML composes the same
    // four top-level keys.
    let yaml = "s: !!set\n? a\n? b\nmetadata:\n  version: 0.1.0 # keep version comment\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    let keys: Vec<String> = mapping
        .keys()
        .map(|k| k.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(
        keys,
        vec!["s", "a", "b", "metadata"],
        "{}",
        debug::tree_to_string(file.syntax())
    );
    assert_eq!(
        mapping
            .get("s")
            .unwrap()
            .as_tagged()
            .unwrap()
            .tag()
            .as_deref(),
        Some("!!set")
    );
    let metadata = mapping.get_mapping("metadata").unwrap();
    metadata.set("version", "0.1.1");
    common::assert_file_cst_ok(&file);
    assert_eq!(
        file.to_string(),
        yaml.replace("version: 0.1.0", "version: 0.1.1")
    );
}

#[test]
fn indented_tagged_set_keeps_entries_nested() {
    let yaml = "s: !!set\n  ? a\n  ? b\nmetadata:\n  version: 0.1.0\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.keys().count(), 2);
    let set = mapping.get("s").unwrap();
    assert_eq!(set.as_tagged().unwrap().tag().as_deref(), Some("!!set"));
}

#[test]
fn tag_followed_by_anchor_keeps_its_indentless_sequence() {
    // `!!seq &a` annotates the sequence just as `!!seq` alone does; the
    // anchor between the tag and the line break must not detach it.
    for value in ["!!seq &a", "&a !!seq"] {
        let yaml = format!("tags: {value}\n- engineering\nmetadata:\n  version: 0.1.0\n");
        let file = YamlFile::from_str(&yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let mapping = file.document().unwrap().as_mapping().unwrap();
        assert_eq!(
            mapping.keys().count(),
            2,
            "{}",
            debug::tree_to_string(file.syntax())
        );
        assert_eq!(
            mapping
                .get("tags")
                .unwrap()
                .as_tagged()
                .unwrap()
                .tag()
                .as_deref(),
            Some("!!seq")
        );
        let metadata = mapping.get_mapping("metadata").unwrap();
        metadata.set("version", "0.1.1");
        common::assert_file_cst_ok(&file);
        assert_eq!(
            file.to_string(),
            yaml.replace("version: 0.1.0", "version: 0.1.1")
        );
    }
}

#[test]
fn tag_anchor_indentless_sequence_is_aliasable() {
    let yaml = "tags: !!seq &a\n- engineering\nnext: *a\n";
    let file = YamlFile::from_str(yaml).unwrap();
    assert_eq!(file.to_string(), yaml);
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.keys().count(), 2);
    assert_eq!(mapping.get("next").unwrap().as_alias().unwrap().name(), "a");
}

#[test]
fn tagged_body_after_a_gap_stays_attached() {
    // A body line with no colon on it -- an explicit key, a block scalar
    // header, a flow collection, a plain scalar -- used to detach from its
    // tag once a blank line intervened, stranding it and every following
    // entry in an ERROR node.
    for body in [
        "  ? ek\n",
        "  |\n    text\n",
        "  >\n    text\n",
        "  [a, b]\n",
        "  {k: v}\n",
        "  plain text\n",
        "  k: v\n",
    ] {
        let yaml = format!("tags: !!seq\n\n{body}sibling: kept\n");
        let file = YamlFile::from_str(&yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let tree = debug::tree_to_string(file.syntax());
        assert!(!tree.contains("ERROR"), "{yaml}\n{tree}");
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let keys: Vec<String> = mapping
            .keys()
            .map(|k| k.as_scalar().unwrap().as_string())
            .collect();
        assert_eq!(keys, vec!["tags", "sibling"], "{yaml}\n{tree}");
    }
}

#[test]
fn tagged_collections_accept_a_trailing_anchor() {
    // `!!set`, `!!omap` and `!!pairs` take their own parse path, which also
    // expected a line break straight after the tag.
    let file = YamlFile::from_str("k: !!set &a\n  ? x\n  ? y\nz: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
        vec!["k", "z"],
        "{}",
        debug::tree_to_string(file.syntax())
    );
    // The anchor must not stop the value being recognised as a set.
    assert_eq!(
        mapping
            .get("k")
            .unwrap()
            .as_tagged()
            .unwrap()
            .as_set()
            .unwrap()
            .len(),
        2
    );

    let file = YamlFile::from_str("k: !!omap &a\n  - x: 1\nz: 1\n").unwrap();
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(
        mapping.keys().map(|k| k.to_string()).collect::<Vec<_>>(),
        vec!["k", "z"],
        "{}",
        debug::tree_to_string(file.syntax())
    );
    assert_eq!(
        mapping
            .get("k")
            .unwrap()
            .as_tagged()
            .unwrap()
            .as_ordered_mapping()
            .unwrap()
            .len(),
        1
    );
}
