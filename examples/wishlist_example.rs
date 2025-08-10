use std::fs;
use yaml_edit::{Document, SyntaxNodeExt, Yaml};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Create test content directly instead of going through files
    // Now we can use unquoted hyphens thanks to the parser fix!
    // URLs still need quotes due to colons being YAML special chars
    let test_content = r#"
Name: example-project
Repository: ["https://github.com/user/repo.git", "https://gitlab.com/user/repo.git"]
Bug-Database: https://github.com/user/repo/issues
"#;

    println!("Original test content:");
    println!("{}", test_content);

    // Parse the test content directly
    let parsed = Yaml::parse(test_content);
    let mut doc = parsed.tree().documents().next().unwrap();

    println!("Loaded document:");
    println!("{}", doc.to_yaml_string());

    // Check and modify fields
    if !doc.contains_key("Repository") {
        doc.set_string("Repository", "https://github.com/user/repo.git");
        println!("Added Repository field");
    } else {
        println!("Repository field exists");
    }

    // Convert arrays to strings - this is the main feature from the wishlist
    if let Some(repo_node) = doc.get("Repository") {
        if repo_node.is_sequence() {
            println!("Repository is a sequence/array");

            if let Some(array_items) = repo_node.as_array() {
                println!("Array items: {:?}", array_items);

                if let Some(first_url) = array_items.get(0) {
                    println!("Converting array to string: {}", first_url);
                    doc.set_string("Repository", first_url);
                }
            }
        } else {
            println!("Repository is not a sequence");
            if let Some(repo_str) = repo_node.as_str() {
                println!("Repository value: {}", repo_str);
            }
        }
    }

    // Set field ordering
    doc.set_field_order(&[
        "Name",
        "Bug-Database",
        "Bug-Submit",
        "Repository",
        "Repository-Browse",
    ]);

    println!("\nAfter processing:");
    println!("{}", doc.to_yaml_string());

    // Save with proper formatting to test the full API
    doc.save_to_file("processed_metadata.yaml")?;
    println!("Saved processed file to processed_metadata.yaml");

    // Also test load_from_file to make sure it works
    let loaded_doc = Document::load_from_file("processed_metadata.yaml")?;
    println!("\nReloaded document:");
    println!("{}", loaded_doc.to_yaml_string());

    // Clean up test file
    fs::remove_file("processed_metadata.yaml").ok();

    Ok(())
}
