use rowan::ast::AstNode;
use yaml_edit::visitor::{
    NodeCounter, ScalarCollector, ScalarTransformer, YamlAccept, YamlVisitor,
};
use yaml_edit::{Document, Yaml};

#[test]
fn test_scalar_collector() {
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

    let parsed = Yaml::parse(yaml_text);
    let yaml = parsed.tree();

    let mut collector = ScalarCollector::new();
    yaml.accept(&mut collector);

    // Should collect all scalar values from the document
    assert!(collector.scalars.contains(&"name".to_string()));
    assert!(collector.scalars.contains(&"John Doe".to_string()));
    assert!(collector.scalars.contains(&"age".to_string()));
    assert!(collector.scalars.contains(&"30".to_string()));
    assert!(collector.scalars.contains(&"address".to_string()));
    assert!(collector.scalars.contains(&"street".to_string()));
    assert!(collector.scalars.contains(&"123 Main St".to_string()));
    assert!(collector.scalars.contains(&"city".to_string()));
    assert!(collector.scalars.contains(&"New York".to_string()));
    assert!(collector.scalars.contains(&"country".to_string()));
    assert!(collector.scalars.contains(&"USA".to_string()));
    assert!(collector.scalars.contains(&"hobbies".to_string()));
    assert!(collector.scalars.contains(&"reading".to_string()));
    assert!(collector.scalars.contains(&"coding".to_string()));
    assert!(collector.scalars.contains(&"hiking".to_string()));
}

#[test]
fn test_node_counter() {
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

    let parsed = Yaml::parse(yaml_text);
    let yaml = parsed.tree();

    let mut counter = NodeCounter::new();
    yaml.accept(&mut counter);

    // Should count all node types
    assert_eq!(counter.document_count, 1);
    assert!(counter.scalar_count > 0); // Should have multiple scalars
    assert!(counter.mapping_count >= 2); // Root mapping and address mapping
    assert_eq!(counter.sequence_count, 1); // hobbies sequence

    // Total should be sum of all counts
    assert_eq!(
        counter.total(),
        counter.document_count
            + counter.scalar_count
            + counter.mapping_count
            + counter.sequence_count
    );
}

#[test]
fn test_scalar_transformer() {
    let yaml_text = r#"
name: john
city: new york
country: usa
"#;

    let parsed = Yaml::parse(yaml_text);
    let yaml = parsed.tree();

    // Transform all scalars to uppercase
    let mut transformer = ScalarTransformer::new(|s: &str| s.to_uppercase());
    yaml.accept(&mut transformer);

    let results = transformer.results();

    // Check that transformations were applied
    assert!(results.contains(&("name".to_string(), "NAME".to_string())));
    assert!(results.contains(&("john".to_string(), "JOHN".to_string())));
    assert!(results.contains(&("city".to_string(), "CITY".to_string())));
    assert!(results.contains(&("new york".to_string(), "NEW YORK".to_string())));
    assert!(results.contains(&("country".to_string(), "COUNTRY".to_string())));
    assert!(results.contains(&("usa".to_string(), "USA".to_string())));
}

#[test]
fn test_visitor_on_sequence() {
    let yaml_text = r#"
- item1
- item2
- nested:
    - subitem1
    - subitem2
"#;

    let parsed = Yaml::parse(yaml_text);
    let yaml = parsed.tree();

    let mut collector = ScalarCollector::new();
    yaml.accept(&mut collector);

    // Should collect all scalars including nested ones
    assert!(collector.scalars.contains(&"item1".to_string()));
    assert!(collector.scalars.contains(&"item2".to_string()));
    assert!(collector.scalars.contains(&"nested".to_string()));
    assert!(collector.scalars.contains(&"subitem1".to_string()));
    assert!(collector.scalars.contains(&"subitem2".to_string()));
}

#[test]
fn test_visitor_on_empty_document() {
    let yaml_text = "";

    let parsed = Yaml::parse(yaml_text);
    let yaml = parsed.tree();

    let mut counter = NodeCounter::new();
    yaml.accept(&mut counter);

    // Empty document should have no nodes (or minimal structure)
    assert_eq!(counter.total(), 0);
}

#[test]
fn test_custom_visitor() {
    // Define a custom visitor that counts only keys in mappings
    struct KeyCounter {
        key_count: usize,
    }

    impl YamlVisitor for KeyCounter {
        fn visit_scalar(&mut self, _scalar: &yaml_edit::Scalar) {
            // Don't count scalars that are not keys
        }

        fn visit_mapping(&mut self, mapping: &yaml_edit::Mapping) {
            // Count keys in this mapping
            for (key, value) in mapping.pairs() {
                if key.is_some() {
                    self.key_count += 1;
                }
                // Recursively visit nested structures
                if let Some(value_node) = value {
                    // VALUE nodes contain the actual content as children
                    for child in value_node.children() {
                        if let Some(nested_mapping) = yaml_edit::Mapping::cast(child.clone()) {
                            nested_mapping.accept(self);
                        } else if let Some(nested_sequence) = yaml_edit::Sequence::cast(child.clone()) {
                            nested_sequence.accept(self);
                        }
                    }
                }
            }
        }

        fn visit_sequence(&mut self, sequence: &yaml_edit::Sequence) {
            // Visit items in sequence to find nested mappings
            for item in sequence.items() {
                if let Some(nested_mapping) = yaml_edit::Mapping::cast(item.clone()) {
                    nested_mapping.accept(self);
                } else if let Some(nested_sequence) = yaml_edit::Sequence::cast(item.clone()) {
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

    let parsed = Yaml::parse(yaml_text);
    let yaml = parsed.tree();

    let mut key_counter = KeyCounter { key_count: 0 };
    yaml.accept(&mut key_counter);

    // Should count: name, age, address, street, city, metadata, created, updated = 8 keys
    assert_eq!(key_counter.key_count, 8);
}

#[test]
fn test_visitor_with_multiple_documents() {
    let yaml_text = r#"
---
doc1: value1
---
doc2: value2
---
doc3: value3
"#;

    let parsed = Yaml::parse(yaml_text);
    let yaml = parsed.tree();

    let mut counter = NodeCounter::new();
    yaml.accept(&mut counter);

    // Should count 3 documents
    assert_eq!(counter.document_count, 3);
    assert!(counter.scalar_count >= 6); // At least 3 keys and 3 values
}
