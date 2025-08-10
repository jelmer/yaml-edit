use std::str::FromStr;
use yaml_edit::{SyntaxNodeExt, Yaml};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml = r#"
Name: example-project
Repository: [https://github.com/user/repo.git, https://gitlab.com/user/repo.git]
Bug-Database: https://github.com/user/repo/issues
"#;

    println!("Testing with:");
    println!("{}", yaml);

    let parsed = Yaml::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();

    println!("Parsed:");
    println!("{}", doc.to_yaml_string());
    println!("Keys: {:?}", doc.keys());

    if let Some(repo_node) = doc.get("Repository") {
        println!("Repository is sequence: {}", repo_node.is_sequence());
        if repo_node.is_sequence() {
            if let Some(array_items) = repo_node.as_array() {
                println!("Repository array items: {:?}", array_items);
            }
        }
    }

    Ok(())
}
