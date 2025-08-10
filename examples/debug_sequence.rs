use yaml_edit::{Document, SyntaxNodeExt, Yaml};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Test with inline sequence syntax
    let test_content1 = r#"Repository: [url1, url2, url3]"#;

    println!("Testing inline array syntax:");
    println!("Content: {}", test_content1);

    let parsed1 = Yaml::parse(test_content1);
    let doc1 = parsed1.tree().documents().next().unwrap();

    println!("Document: {}", doc1.to_yaml_string());

    if let Some(repo_node) = doc1.get("Repository") {
        println!("Found Repository node: {:?}", repo_node.kind());
        println!("Is sequence: {}", repo_node.is_sequence());

        if let Some(array_items) = repo_node.as_array() {
            println!("Array items: {:?}", array_items);
        }

        if let Some(repo_str) = repo_node.as_str() {
            println!("As string: {}", repo_str);
        }
    }

    println!("\n---\n");

    // Test with block sequence syntax
    let test_content2 = r#"Repository:
  - url1
  - url2
  - url3"#;

    println!("Testing block array syntax:");
    println!("Content: {}", test_content2);

    let parsed2 = Yaml::parse(test_content2);
    let doc2 = parsed2.tree().documents().next().unwrap();

    println!("Document: {}", doc2.to_yaml_string());

    if let Some(repo_node) = doc2.get("Repository") {
        println!("Found Repository node: {:?}", repo_node.kind());
        println!("Is sequence: {}", repo_node.is_sequence());

        if let Some(array_items) = repo_node.as_array() {
            println!("Array items: {:?}", array_items);
        }

        if let Some(repo_str) = repo_node.as_str() {
            println!("As string: {}", repo_str);
        }
    }

    Ok(())
}
