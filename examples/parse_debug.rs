use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let test_content = r#"Name: example-project
Repository: [https://github.com/user/repo.git, https://gitlab.com/user/repo.git]
Bug-Database: https://github.com/user/repo/issues
"#;

    println!("Original test content:");
    println!("{}", test_content);

    let parsed = Yaml::parse(test_content);
    println!("\nParsed successfully: {}", !parsed.has_errors());

    if parsed.has_errors() {
        println!("Parse errors: {:?}", parsed.errors());
    }

    println!("\nFull parsed tree:");
    println!("'{}'", parsed.tree().to_string());

    let documents: Vec<_> = parsed.tree().documents().collect();
    println!("\nNumber of documents: {}", documents.len());

    for (i, doc) in documents.iter().enumerate() {
        println!("Document {}: '{}'", i, doc.to_yaml_string());
    }

    Ok(())
}
