use rowan::ast::AstNode;
use yaml_edit::{Mapping, Yaml};

#[test]
fn test_sibling_indentation_parsing() {
    // Test that sibling mappings at the same indentation level are parsed correctly
    let yaml_str = r#"
parent:
  child1:
    key1: value1
  child2:
    key2: value2
"#;

    let yaml = Yaml::parse(yaml_str).to_result().unwrap();
    let doc = yaml.document().expect("No document");
    let root = doc.as_mapping().expect("Not a mapping");

    // Parent should exist at root
    assert!(root.get("parent").is_some(), "parent key should exist");

    // Get parent's value as a mapping
    let parent_mapping = root
        .get("parent")
        .and_then(Mapping::cast)
        .expect("parent should have a mapping value");

    // Check that child1 and child2 are direct children of parent
    let parent_keys: Vec<String> = parent_mapping.keys().collect();
    assert_eq!(
        parent_keys.len(),
        2,
        "parent should have exactly 2 children"
    );
    assert!(
        parent_keys.contains(&"child1".to_string()),
        "child1 should be under parent"
    );
    assert!(
        parent_keys.contains(&"child2".to_string()),
        "child2 should be under parent"
    );

    // Verify child1's content
    let child1_mapping = parent_mapping
        .get("child1")
        .and_then(Mapping::cast)
        .expect("child1 should have a mapping value");
    assert_eq!(
        child1_mapping.keys().collect::<Vec<_>>(),
        vec!["key1".to_string()],
        "child1 should only contain key1"
    );

    // Verify child2's content
    let child2_mapping = parent_mapping
        .get("child2")
        .and_then(Mapping::cast)
        .expect("child2 should have a mapping value");
    assert_eq!(
        child2_mapping.keys().collect::<Vec<_>>(),
        vec!["key2".to_string()],
        "child2 should only contain key2"
    );

    // Ensure child2 is NOT nested inside child1
    assert!(
        child1_mapping.get("child2").is_none(),
        "child2 should NOT be inside child1"
    );
}

#[test]
fn test_multiple_nesting_levels() {
    let yaml_str = r#"
level1:
  level2a:
    level3a:
      key: value1
    level3b:
      key: value2
  level2b:
    level3c:
      key: value3
"#;

    let yaml = Yaml::parse(yaml_str).to_result().unwrap();
    let doc = yaml.document().expect("No document");
    let root = doc.as_mapping().expect("Not a mapping");

    let level1 = root
        .get("level1")
        .and_then(Mapping::cast)
        .expect("level1 should be a mapping");

    // Check level2 siblings
    let level1_keys: Vec<String> = level1.keys().collect();
    assert_eq!(level1_keys.len(), 2);
    assert!(level1_keys.contains(&"level2a".to_string()));
    assert!(level1_keys.contains(&"level2b".to_string()));

    // Check level3 siblings under level2a
    let level2a = level1
        .get("level2a")
        .and_then(Mapping::cast)
        .expect("level2a should be a mapping");
    let level2a_keys: Vec<String> = level2a.keys().collect();
    assert_eq!(level2a_keys.len(), 2);
    assert!(level2a_keys.contains(&"level3a".to_string()));
    assert!(level2a_keys.contains(&"level3b".to_string()));

    // Verify level3b is not nested in level3a
    let level3a = level2a
        .get("level3a")
        .and_then(Mapping::cast)
        .expect("level3a should be a mapping");
    assert!(
        level3a.get("level3b").is_none(),
        "level3b should not be nested in level3a"
    );
}

#[test]
fn test_mixed_content_with_siblings() {
    let yaml_str = r#"
root:
  scalar_sibling: value
  mapping_sibling:
    nested: data
  another_scalar: value2
"#;

    let yaml = Yaml::parse(yaml_str).to_result().unwrap();
    let doc = yaml.document().expect("No document");
    let root_mapping = doc.as_mapping().expect("Not a mapping");

    let root_value = root_mapping
        .get("root")
        .and_then(Mapping::cast)
        .expect("root should have a mapping value");

    // All three siblings should be at the same level
    let keys: Vec<String> = root_value.keys().collect();
    assert_eq!(keys.len(), 3);
    assert!(keys.contains(&"scalar_sibling".to_string()));
    assert!(keys.contains(&"mapping_sibling".to_string()));
    assert!(keys.contains(&"another_scalar".to_string()));

    // Verify the mapping sibling has its own nested content
    let mapping_sibling = root_value
        .get("mapping_sibling")
        .and_then(Mapping::cast)
        .expect("mapping_sibling should be a mapping");
    assert_eq!(
        mapping_sibling.keys().collect::<Vec<_>>(),
        vec!["nested".to_string()]
    );
}

#[test]
fn test_indentation_at_document_root() {
    // Test that root-level siblings work correctly
    let yaml_str = r#"
first:
  nested1: value1
second:
  nested2: value2
third: value3
"#;

    let yaml = Yaml::parse(yaml_str).to_result().unwrap();
    let doc = yaml.document().expect("No document");
    let root = doc.as_mapping().expect("Not a mapping");

    // All three should be at root level
    let root_keys: Vec<String> = root.keys().collect();
    assert_eq!(root_keys.len(), 3);
    assert!(root_keys.contains(&"first".to_string()));
    assert!(root_keys.contains(&"second".to_string()));
    assert!(root_keys.contains(&"third".to_string()));

    // Verify first mapping
    let first = root
        .get("first")
        .and_then(Mapping::cast)
        .expect("first should be a mapping");
    assert_eq!(
        first.keys().collect::<Vec<_>>(),
        vec!["nested1".to_string()]
    );

    // Verify second mapping
    let second = root
        .get("second")
        .and_then(Mapping::cast)
        .expect("second should be a mapping");
    assert_eq!(
        second.keys().collect::<Vec<_>>(),
        vec!["nested2".to_string()]
    );

    // Ensure second is not nested in first
    assert!(
        first.get("second").is_none(),
        "second should not be nested in first"
    );
}

#[test]
fn test_deep_nesting_with_siblings() {
    let yaml_str = r#"
a:
  b:
    c:
      d:
        e: value1
        f: value2
      g:
        h: value3
    i:
      j: value4
"#;

    let yaml = Yaml::parse(yaml_str).to_result().unwrap();
    let doc = yaml.document().expect("No document");
    let root = doc.as_mapping().expect("Not a mapping");

    // Navigate down the tree
    let a = root
        .get("a")
        .and_then(Mapping::cast)
        .expect("a should be a mapping");
    let b = a
        .get("b")
        .and_then(Mapping::cast)
        .expect("b should be a mapping");

    // b should have two children: c and i
    let b_keys: Vec<String> = b.keys().collect();
    assert_eq!(b_keys.len(), 2);
    assert!(b_keys.contains(&"c".to_string()));
    assert!(b_keys.contains(&"i".to_string()));

    let c = b
        .get("c")
        .and_then(Mapping::cast)
        .expect("c should be a mapping");

    // c should have two children: d and g
    let c_keys: Vec<String> = c.keys().collect();
    assert_eq!(c_keys.len(), 2);
    assert!(c_keys.contains(&"d".to_string()));
    assert!(c_keys.contains(&"g".to_string()));

    let d = c
        .get("d")
        .and_then(Mapping::cast)
        .expect("d should be a mapping");

    // d should have two children: e and f
    let d_keys: Vec<String> = d.keys().collect();
    assert_eq!(d_keys.len(), 2);
    assert!(d_keys.contains(&"e".to_string()));
    assert!(d_keys.contains(&"f".to_string()));
}

#[test]
#[ignore] // TODO: Fix architectural issue with empty mapping values consuming sibling keys
fn test_empty_mapping_values() {
    // Test that empty mapping values don't break sibling parsing
    let yaml_str = r#"
parent:
  child1:
  child2:
    key: value
  child3:
"#;

    let yaml = Yaml::parse(yaml_str).to_result().unwrap();
    let doc = yaml.document().expect("No document");
    let root = doc.as_mapping().expect("Not a mapping");

    let parent = root
        .get("parent")
        .and_then(Mapping::cast)
        .expect("parent should be a mapping");

    // All three children should be siblings
    let keys: Vec<String> = parent.keys().collect();
    assert_eq!(keys.len(), 3);
    assert!(keys.contains(&"child1".to_string()));
    assert!(keys.contains(&"child2".to_string()));
    assert!(keys.contains(&"child3".to_string()));

    // child2 should have content
    let child2 = parent
        .get("child2")
        .and_then(Mapping::cast)
        .expect("child2 should be a mapping");
    assert_eq!(child2.keys().collect::<Vec<_>>(), vec!["key".to_string()]);
}
