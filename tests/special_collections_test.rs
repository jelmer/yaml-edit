use rowan::ast::AstNode;
use std::collections::BTreeSet;
use std::str::FromStr;
use yaml_edit::*;

#[test]
fn test_set_parsing() {
    let yaml_content = r#"!!set
  item1: null
  item2: null
  item3: null
"#;

    let yaml = Yaml::from_str(yaml_content).expect("Failed to parse YAML");
    let doc = yaml.document().expect("No document found");
    let root = doc.root_node().expect("No root value");

    // Debug: print the structure
    println!("Root node kind: {:?}", root.kind());
    println!("Root node children:");
    for child in root.children() {
        println!("  Child kind: {:?}, text: {:?}", child.kind(), child.text());
        // Check if it's a mapping by trying to cast it
        if let Some(mapping) = Mapping::cast(child.clone()) {
            println!("    Found mapping! Keys:");
            for (key, value) in mapping.pairs() {
                if let (Some(k), Some(v)) = (key, value) {
                    println!("      Key: {:?}, Value: {:?}", k.value(), v.text());
                }
            }
        }
    }

    // Test via TaggedScalar interface
    if let Some(tagged_scalar) = TaggedScalar::cast(root.clone()) {
        println!("Tagged scalar found!");
        println!("Tag: {:?}", tagged_scalar.tag());

        println!("Tagged scalar children:");
        for child in tagged_scalar.syntax().children() {
            println!("  Child kind: {:?}, text: {:?}", child.kind(), child.text());
        }

        assert_eq!(tagged_scalar.tag(), Some("!!set".to_string()));
        let set = tagged_scalar.as_set().expect("Should parse as set");
        assert_eq!(set.len(), 3);
        assert!(set.contains("item1"));
        assert!(set.contains("item2"));
        assert!(set.contains("item3"));
    } else {
        panic!("Expected tagged scalar for set");
    }
}

#[test]
fn test_set_flow_syntax() {
    let yaml_content = "!!set { item1: null, item2: null, item3: null }";

    println!("Flow syntax debug:");
    let yaml = Yaml::from_str(yaml_content).expect("Failed to parse YAML");
    let doc = yaml.document().expect("No document found");
    let root = doc.root_node().expect("No root value");
    println!("Root node text: {:?}", root.text());
    for child in root.children() {
        println!("  Child kind: {:?}, text: {:?}", child.kind(), child.text());
    }

    let yaml = Yaml::from_str(yaml_content).expect("Failed to parse YAML");
    let doc = yaml.document().expect("No document found");
    let root = doc.root_node().expect("No root value");

    if let Some(tagged_scalar) = TaggedScalar::cast(root) {
        assert_eq!(tagged_scalar.tag(), Some("!!set".to_string()));
        let set = tagged_scalar.as_set().expect("Should parse as set");
        assert_eq!(set.len(), 3);
        assert!(set.contains("item1"));
        assert!(set.contains("item2"));
        assert!(set.contains("item3"));
    } else {
        panic!("Expected tagged scalar for set");
    }
}

#[test]
fn test_omap_parsing() {
    let yaml_content = r#"!!omap
- first: value1
- second: value2
- third: value3
"#;

    let yaml = Yaml::from_str(yaml_content).expect("Failed to parse YAML");
    let doc = yaml.document().expect("No document found");
    let root = doc.root_node().expect("No root value");

    if let Some(tagged_scalar) = TaggedScalar::cast(root) {
        assert_eq!(tagged_scalar.tag(), Some("!!omap".to_string()));
        let omap = tagged_scalar
            .as_ordered_mapping()
            .expect("Should parse as ordered mapping");
        assert_eq!(omap.len(), 3);
        assert_eq!(omap[0].0, "first");
        assert_eq!(omap[1].0, "second");
        assert_eq!(omap[2].0, "third");

        // Check values
        if let Some(scalar) = omap[0].1.as_scalar() {
            assert_eq!(scalar.to_yaml_string(), "value1");
        }
    } else {
        panic!("Expected tagged scalar for omap");
    }
}

#[test]
fn test_omap_flow_syntax() {
    let yaml_content = "!!omap [ { first: value1 }, { second: value2 }, { third: value3 } ]";

    let yaml = Yaml::from_str(yaml_content).expect("Failed to parse YAML");
    let doc = yaml.document().expect("No document found");
    let root = doc.root_node().expect("No root value");

    if let Some(tagged_scalar) = TaggedScalar::cast(root) {
        assert_eq!(tagged_scalar.tag(), Some("!!omap".to_string()));
        let omap = tagged_scalar
            .as_ordered_mapping()
            .expect("Should parse as ordered mapping");
        assert_eq!(omap.len(), 3);
        assert_eq!(omap[0].0, "first");
        assert_eq!(omap[1].0, "second");
        assert_eq!(omap[2].0, "third");
    } else {
        panic!("Expected tagged scalar for omap");
    }
}

#[test]
fn test_pairs_parsing() {
    let yaml_content = r#"!!pairs
- key1: value1
- key1: value2
- key2: value3
"#;

    let yaml = Yaml::from_str(yaml_content).expect("Failed to parse YAML");
    let doc = yaml.document().expect("No document found");
    let root = doc.root_node().expect("No root value");

    if let Some(tagged_scalar) = TaggedScalar::cast(root) {
        assert_eq!(tagged_scalar.tag(), Some("!!pairs".to_string()));
        let pairs = tagged_scalar.as_pairs().expect("Should parse as pairs");
        assert_eq!(pairs.len(), 3);
        assert_eq!(pairs[0].0, "key1");
        assert_eq!(pairs[1].0, "key1"); // Duplicate key allowed
        assert_eq!(pairs[2].0, "key2");
    } else {
        panic!("Expected tagged scalar for pairs");
    }
}

#[test]
fn test_pairs_flow_syntax() {
    let yaml_content = "!!pairs [ { key1: value1 }, { key1: value2 }, { key2: value3 } ]";

    let yaml = Yaml::from_str(yaml_content).expect("Failed to parse YAML");
    let doc = yaml.document().expect("No document found");
    let root = doc.root_node().expect("No root value");

    if let Some(tagged_scalar) = TaggedScalar::cast(root) {
        assert_eq!(tagged_scalar.tag(), Some("!!pairs".to_string()));
        let pairs = tagged_scalar.as_pairs().expect("Should parse as pairs");
        assert_eq!(pairs.len(), 3);
        assert_eq!(pairs[0].0, "key1");
        assert_eq!(pairs[1].0, "key1"); // Duplicate key allowed
        assert_eq!(pairs[2].0, "key2");
    } else {
        panic!("Expected tagged scalar for pairs");
    }
}

#[test]
fn test_yaml_value_special_collections() {
    // Test YamlValue creation and serialization
    let mut set = BTreeSet::new();
    set.insert("apple".to_string());
    set.insert("banana".to_string());
    set.insert("cherry".to_string());

    let set_value = YamlValue::from_set(set);
    assert!(set_value.is_set());
    let yaml_str = set_value.to_yaml_string(0);
    assert!(yaml_str.contains("!!set"));
    assert!(yaml_str.contains("apple: null"));
    assert!(yaml_str.contains("banana: null"));
    assert!(yaml_str.contains("cherry: null"));

    // Test ordered mapping
    let omap_pairs = vec![
        ("first".to_string(), YamlValue::from("alpha")),
        ("second".to_string(), YamlValue::from("beta")),
        ("third".to_string(), YamlValue::from("gamma")),
    ];

    let omap_value = YamlValue::from_ordered_mapping(omap_pairs);
    assert!(omap_value.is_ordered_mapping());
    let yaml_str = omap_value.to_yaml_string(0);
    assert!(yaml_str.contains("!!omap"));
    assert!(yaml_str.contains("- first: alpha"));
    assert!(yaml_str.contains("- second: beta"));
    assert!(yaml_str.contains("- third: gamma"));

    // Test pairs
    let pairs_data = vec![
        ("name".to_string(), YamlValue::from("Alice")),
        ("name".to_string(), YamlValue::from("Bob")),
        ("age".to_string(), YamlValue::from(30)),
    ];

    let pairs_value = YamlValue::from_pairs(pairs_data);
    assert!(pairs_value.is_pairs());
    let yaml_str = pairs_value.to_yaml_string(0);
    assert!(yaml_str.contains("!!pairs"));
    assert!(yaml_str.contains("- name: Alice"));
    assert!(yaml_str.contains("- name: Bob"));
    assert!(yaml_str.contains("- age: 30"));
}

#[test]
fn test_empty_special_collections() {
    // Test empty collections
    let empty_set = YamlValue::set();
    assert_eq!(empty_set.to_yaml_string(0), "!!set {}");

    let empty_omap = YamlValue::ordered_mapping();
    assert_eq!(empty_omap.to_yaml_string(0), "!!omap []");

    let empty_pairs = YamlValue::pairs();
    assert_eq!(empty_pairs.to_yaml_string(0), "!!pairs []");
}
