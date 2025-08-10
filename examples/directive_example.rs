use std::str::FromStr;
use yaml_edit::*;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== YAML Directive Support Example ===\n");

    // Example 1: Parse YAML with directives
    println!("1. Parsing YAML with directives:");
    let yaml_content = r#"%YAML 1.2
%TAG ! tag:example.com,2000:app/
---
name: my-application
version: 1.0.0
"#;

    let parsed = Yaml::from_str(yaml_content)?;
    println!("Input:\n{}", yaml_content);

    // Access directives
    let directives: Vec<_> = parsed.directives().collect();
    println!("Found {} directives:", directives.len());
    for directive in &directives {
        println!("  - {}", directive.text());
        if let Some(name) = directive.name() {
            println!("    Name: {}", name);
            if let Some(value) = directive.value() {
                println!("    Value: {}", value);
            }
        }
    }

    // Example 2: Create YAML with directives programmatically
    println!("\n2. Creating YAML with directives programmatically:");
    let mut yaml = Yaml::new();

    // Add YAML version directive
    yaml.add_directive("%YAML 1.2");

    // Add TAG directive
    yaml.add_directive("%TAG ! tag:example.com,2000:app/");

    // Create a document
    let mut doc = Document::new_mapping();
    doc.set_string("application", "yaml-edit");
    doc.set_string("version", "0.1.0");
    doc.set_string("author", "Jelmer Vernooĳ");

    yaml.push_document(doc);

    println!("Generated YAML:\n{}", yaml.to_string());

    // Example 3: Working with different directive types
    println!("\n3. Creating specific directive types:");

    let yaml_dir = Directive::new_yaml_version("1.2");
    println!("YAML version directive: {}", yaml_dir.text());
    println!("Is YAML version: {}", yaml_dir.is_yaml_version());

    let tag_dir = Directive::new_tag("!local!", "tag:local.example.com,2000:");
    println!("TAG directive: {}", tag_dir.text());
    println!("Is TAG directive: {}", tag_dir.is_tag());

    // Example 4: Document preservation
    println!("\n4. Document preservation test:");
    let complex_yaml = r#"%YAML 1.2
%TAG !local! tag:local.example.com,2000:
%TAG !example! tag:example.com,2000:
---
name: !local!string "my-app"
version: "1.0"
dependencies: !example!list
  - yaml-edit
  - serde
---
second_doc: value
"#;

    let parsed_complex = Yaml::from_str(complex_yaml)?;
    println!("Original:\n{}", complex_yaml);
    println!("Parsed and serialized:\n{}", parsed_complex.to_string());

    let dirs: Vec<_> = parsed_complex.directives().collect();
    println!("Preserved {} directives", dirs.len());
    let docs: Vec<_> = parsed_complex.documents().collect();
    println!("Preserved {} documents", docs.len());

    Ok(())
}
