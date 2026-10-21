//! Walking parsed documents with a `YamlVisitor`: which nodes a walk
//! visits, in which order, and what a visitor can override.

use std::str::FromStr;
use yaml_edit::visitor::{
    NodeCounter, ScalarCollector, ScalarTransformer, YamlAccept, YamlVisitor,
};
use yaml_edit::{Document, Mapping, Scalar, Sequence, TaggedNode, YamlFile};

struct ScalarNames {
    seen: Vec<String>,
}

impl YamlVisitor for ScalarNames {
    fn visit_scalar(&mut self, scalar: &Scalar) {
        self.seen.push(scalar.as_string());
    }
}

fn visited_scalars(src: &str) -> Vec<String> {
    let doc = Document::from_str(src).unwrap();
    let mut v = ScalarNames { seen: vec![] };
    doc.accept(&mut v);
    v.seen
}

#[test]
fn walks_into_tagged_nodes() {
    // A tag used to stop traversal dead: walk_sequence cast the item
    // straight to Scalar/Mapping/Sequence, and the mapping walk unwrapped
    // only KEY/VALUE, so everything under a TAGGED_NODE went unvisited.
    assert_eq!(visited_scalars("- !!str a\n- b\n"), ["a", "b"]);
    assert_eq!(visited_scalars("k: !!str v\n"), ["k", "v"]);
    assert_eq!(visited_scalars("!!str k: v\n"), ["k", "v"]);
    assert_eq!(visited_scalars("k: !!map\n  a: 1\n"), ["k", "a", "1"]);
    assert_eq!(visited_scalars("- !!seq\n  - a\n"), ["a"]);
}

#[test]
fn visit_tagged_node_can_be_overridden() {
    struct Tags {
        tags: Vec<String>,
    }
    impl YamlVisitor for Tags {
        fn visit_tagged_node(&mut self, tagged: &TaggedNode) {
            self.tags.push(tagged.tag().unwrap_or_default());
            // Deliberately do not descend.
        }
    }
    let doc = Document::from_str("- !!str a\n- !custom b\n").unwrap();
    let mut v = Tags { tags: vec![] };
    doc.accept(&mut v);
    assert_eq!(v.tags, ["!!str", "!custom"]);
}

#[test]
fn scalar_collector() {
    let yaml_text = r#"
name: John Doe
age: 30
address:
  street: 123 Main St
  city: New York
  country: USA
hobbies:
  - reading
  - coding
  - hiking
"#;

    let parsed = YamlFile::parse(yaml_text);
    let yaml = parsed.tree();

    let mut collector = ScalarCollector::new();
    yaml.accept(&mut collector);

    // Should collect all scalar values from the document
    assert_eq!(
        collector.scalars,
        vec![
            "name",
            "John Doe",
            "age",
            "30",
            "address",
            "street",
            "123 Main St",
            "city",
            "New York",
            "country",
            "USA",
            "hobbies",
            "reading",
            "coding",
            "hiking",
        ]
    );
}

#[test]
fn node_counter() {
    let yaml_text = r#"
name: John Doe
age: 30
address:
  street: 123 Main St
  city: New York
  country: USA
hobbies:
  - reading
  - coding
  - hiking
"#;

    let parsed = YamlFile::parse(yaml_text);
    let yaml = parsed.tree();

    let mut counter = NodeCounter::new();
    yaml.accept(&mut counter);

    // Should count all node types
    assert_eq!(counter.document_count, 1);
    assert_eq!(counter.scalar_count, 15);
    assert_eq!(counter.mapping_count, 2); // Root mapping and address mapping
    assert_eq!(counter.sequence_count, 1); // hobbies sequence

    assert_eq!(counter.total(), 19);
}

#[test]
fn scalar_transformer() {
    let yaml_text = r#"
name: john
city: new york
country: usa
"#;

    let parsed = YamlFile::parse(yaml_text);
    let yaml = parsed.tree();

    // Transform all scalars to uppercase
    let mut transformer = ScalarTransformer::new(|s: &str| s.to_uppercase());
    yaml.accept(&mut transformer);

    let results = transformer.results();

    // Check that transformations were applied
    assert_eq!(
        results,
        &[
            ("name".to_string(), "NAME".to_string()),
            ("john".to_string(), "JOHN".to_string()),
            ("city".to_string(), "CITY".to_string()),
            ("new york".to_string(), "NEW YORK".to_string()),
            ("country".to_string(), "COUNTRY".to_string()),
            ("usa".to_string(), "USA".to_string()),
        ]
    );
}

#[test]
fn visitor_on_sequence() {
    let yaml_text = r#"
- item1
- item2
- nested:
    - subitem1
    - subitem2
"#;

    let parsed = YamlFile::parse(yaml_text);
    let yaml = parsed.tree();

    let mut collector = ScalarCollector::new();
    yaml.accept(&mut collector);

    // Should collect all scalars including nested ones
    assert_eq!(
        collector.scalars,
        vec!["item1", "item2", "nested", "subitem1", "subitem2"]
    );
}

#[test]
fn visitor_on_empty_document() {
    let yaml_text = "";

    let parsed = YamlFile::parse(yaml_text);
    let yaml = parsed.tree();

    let mut counter = NodeCounter::new();
    yaml.accept(&mut counter);

    // Empty document should have no nodes (or minimal structure)
    assert_eq!(counter.total(), 0);
}

#[test]
fn custom_visitor() {
    // Define a custom visitor that counts only keys in mappings
    struct KeyCounter {
        key_count: usize,
    }

    impl YamlVisitor for KeyCounter {
        fn visit_scalar(&mut self, _scalar: &Scalar) {
            // Don't count scalars that are not keys
        }

        fn visit_mapping(&mut self, mapping: &Mapping) {
            // Count keys in this mapping
            for (_key, value) in mapping.iter() {
                self.key_count += 1;
                // Recursively visit nested structures
                if let Some(nested_mapping) = value.as_mapping() {
                    nested_mapping.accept(self);
                } else if let Some(nested_sequence) = value.as_sequence() {
                    nested_sequence.accept(self);
                }
            }
        }

        fn visit_sequence(&mut self, sequence: &Sequence) {
            // Visit items in sequence to find nested mappings
            for value in sequence.values() {
                if let Some(nested_mapping) = value.as_mapping() {
                    nested_mapping.accept(self);
                } else if let Some(nested_sequence) = value.as_sequence() {
                    nested_sequence.accept(self);
                }
            }
        }
    }

    let yaml_text = r#"
name: John
age: 30
address:
  street: 123 Main St
  city: New York
metadata:
  created: 2024-01-01
  updated: 2024-01-02
"#;

    let parsed = YamlFile::parse(yaml_text);
    let yaml = parsed.tree();

    let mut key_counter = KeyCounter { key_count: 0 };
    yaml.accept(&mut key_counter);

    // Should count: name, age, address, street, city, metadata, created, updated = 8 keys
    assert_eq!(key_counter.key_count, 8);
}

#[test]
fn visitor_with_multiple_documents() {
    let yaml_text = r#"
---
doc1: value1
---
doc2: value2
---
doc3: value3
"#;

    let parsed = YamlFile::parse(yaml_text);
    let yaml = parsed.tree();

    let mut counter = NodeCounter::new();
    yaml.accept(&mut counter);

    // Should count 3 documents
    assert_eq!(counter.document_count, 3);
    assert_eq!(counter.scalar_count, 6);
    assert_eq!(counter.mapping_count, 3);
}

#[test]
fn visit_tagged_root() {
    struct Tags {
        tags: Vec<String>,
    }
    impl YamlVisitor for Tags {
        fn visit_tagged_node(&mut self, tagged: &TaggedNode) {
            self.tags.push(tagged.tag().unwrap_or_default());
        }
    }
    for (src, tag) in [
        ("--- !custom {a: 1}\n", "!custom"),
        ("--- !!seq [a]\n", "!!seq"),
        ("--- !!str a\n", "!!str"),
    ] {
        let mut v = Tags { tags: vec![] };
        Document::from_str(src).unwrap().accept(&mut v);
        assert_eq!(v.tags, [tag], "{src:?}");
    }
    let doc = Document::from_str("--- !custom {a: !!str 1}\n").unwrap();
    let mut counter = NodeCounter::new();
    doc.accept(&mut counter);
    assert_eq!(
        (
            counter.document_count,
            counter.mapping_count,
            counter.scalar_count
        ),
        (1, 1, 2)
    );
    assert_eq!(visited_scalars("--- !!seq [a, b]\n"), ["a", "b"]);
}

#[test]
fn walks_complex_keys_and_nested_sequences() {
    assert_eq!(
        visited_scalars("? [a, b]\n: 1\n? {k: v}\n: 2\n!t c: 3\n"),
        ["a", "b", "1", "k", "v", "2", "c", "3"]
    );
    assert_eq!(visited_scalars("- [a, [b]]\n- c\n"), ["a", "b", "c"]);
}

#[test]
fn accept_on_scalar_and_sequence() {
    let doc = Document::from_str("- a\n- [b, c]\n").unwrap();
    let sequence = doc.as_sequence().unwrap();
    let mut counter = NodeCounter::default();
    sequence.accept(&mut counter);
    assert_eq!(
        (
            counter.document_count,
            counter.sequence_count,
            counter.scalar_count
        ),
        (0, 2, 3)
    );
    let mut collector = ScalarCollector::default();
    sequence
        .get(0)
        .unwrap()
        .as_scalar()
        .unwrap()
        .accept(&mut collector);
    assert_eq!(collector.scalars, ["a"]);
}
