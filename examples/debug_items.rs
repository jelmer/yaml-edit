use yaml_edit::{SyntaxNodeExt, Yaml};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let test_content = r#"Repository: [url1, url2, url3]"#;

    println!("Testing with content: {}", test_content);

    let parsed = Yaml::parse(test_content);
    let doc = parsed.tree().documents().next().unwrap();

    if let Some(repo_node) = doc.get("Repository") {
        println!("Found Repository node: {:?}", repo_node.kind());
        println!("Is sequence: {}", repo_node.is_sequence());

        let items = repo_node.sequence_items();
        println!("Number of sequence items: {}", items.len());

        for (i, item) in items.iter().enumerate() {
            println!(
                "Item {}: kind={:?}, text='{}'",
                i,
                item.kind(),
                item.text().to_string()
            );

            // Try different ways to get string value
            if let Some(as_str) = item.as_str() {
                println!("  as_str: '{}'", as_str);
            } else {
                println!("  as_str: None");
            }

            // Also try direct text access
            println!("  direct text: '{}'", item.text().to_string());
        }
    }

    Ok(())
}
