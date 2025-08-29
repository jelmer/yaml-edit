use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{Mapping, Scalar, Sequence, Yaml};

fn main() {
    let yaml = r#"
- item1: value1
- item2 has text: but not a key
- item3
"#;

    println!("Testing YAML:");
    println!("{}", yaml);

    let parsed = Yaml::from_str(yaml).expect("Failed to parse YAML");
    let doc = parsed.document().expect("Should have a document");
    let sequence = doc.as_sequence().expect("Root should be a sequence");

    let items: Vec<_> = sequence.items().collect();
    println!("Found {} items", items.len());

    for (i, item) in items.iter().enumerate() {
        println!(
            "\nItem {}: kind={:?}, text='{}'",
            i,
            item.kind(),
            item.text()
        );

        // Check what children this item has
        for child in item.children() {
            println!("  Child: kind={:?}, text='{}'", child.kind(), child.text());

            if let Some(mapping) = Mapping::cast(child.clone()) {
                println!("    -> This is a Mapping");
                for (key, value) in mapping.pairs() {
                    if let (Some(k), Some(v)) = (key, value) {
                        println!("      Key: '{}', Value: '{}'", k.text(), v.text());
                    }
                }
            } else if let Some(scalar) = Scalar::cast(child.clone()) {
                println!("    -> This is a Scalar: '{}'", scalar.as_string());
            } else {
                println!("    -> Unknown type");
            }
        }
    }
}
