use rowan::ast::AstNode;
use yaml_edit::visitor::{NodeCounter, ScalarCollector, ScalarTransformer, YamlAccept};
use yaml_edit::Yaml;

fn main() {
    // Example 1: Count nodes in a YAML document
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
metadata:
  created: 2024-01-01
  updated: 2024-01-02
"#;

    let parsed = Yaml::parse(yaml_text);
    let yaml = parsed.tree();

    // Count nodes
    let mut counter = NodeCounter::new();
    yaml.accept(&mut counter);

    println!("=== Node Counter Results ===");
    println!("Documents: {}", counter.document_count);
    println!("Scalars: {}", counter.scalar_count);
    println!("Mappings: {}", counter.mapping_count);
    println!("Sequences: {}", counter.sequence_count);
    println!("Total nodes: {}", counter.total());
    println!();

    // Collect all scalars
    let mut collector = ScalarCollector::new();
    yaml.accept(&mut collector);

    println!("=== Collected Scalars ===");
    for (i, scalar) in collector.scalars.iter().enumerate() {
        println!("{}: {}", i + 1, scalar);
    }
    println!();

    // Transform scalars
    let mut transformer = ScalarTransformer::new(|s: &str| {
        if s.chars().all(|c| c.is_alphabetic() || c.is_whitespace()) {
            s.to_uppercase()
        } else {
            s.to_string()
        }
    });
    yaml.accept(&mut transformer);

    println!("=== Transformed Scalars (alphabetic to uppercase) ===");
    for (original, transformed) in transformer.results() {
        if original != transformed {
            println!("{} -> {}", original, transformed);
        }
    }
}
