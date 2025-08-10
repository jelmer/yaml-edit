use std::str::FromStr;
use yaml_edit::{SyntaxNodeExt, Yaml};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let test_content = r#"Name: example-project
Repository: [https://github.com/user/repo.git, https://gitlab.com/user/repo.git]
Bug-Database: https://github.com/user/repo/issues
"#;

    println!("Original test content:");
    println!("{}", test_content);

    let parsed = Yaml::from_str(test_content)?;
    println!("\nParsed successfully!");

    let doc = parsed.document().unwrap();

    println!("\nDocument content:");
    println!("'{}'", doc.to_yaml_string());

    println!("\nDocument keys: {:?}", doc.keys());

    if let Some(name_node) = doc.get("Name") {
        println!("Name field found: '{}'", name_node.text().to_string());
        if let Some(name_str) = name_node.as_str() {
            println!("Name as string: '{}'", name_str);
        }
    }

    if let Some(repo_node) = doc.get("Repository") {
        println!("Repository field found: '{}'", repo_node.text().to_string());
        println!("Repository is sequence: {}", repo_node.is_sequence());

        if repo_node.is_sequence() {
            if let Some(array_items) = repo_node.as_array() {
                println!("Repository array items: {:?}", array_items);
            }
        } else if let Some(repo_str) = repo_node.as_str() {
            println!("Repository as string: '{}'", repo_str);
        }
    }

    Ok(())
}
