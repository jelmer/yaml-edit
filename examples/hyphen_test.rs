use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml1 = r#"
Name: example_project
Repository: something
"#;
    println!("Test 1 - underscore:");
    let parsed1 = Yaml::from_str(yaml1).unwrap();
    let doc1 = parsed1.document().unwrap();
    println!("{}", doc1.to_yaml_string());

    let yaml2 = r#"
Name: "example-project"
Repository: something
"#;
    println!("\nTest 2 - quoted hyphen:");
    let parsed2 = Yaml::from_str(yaml2).unwrap();
    let doc2 = parsed2.document().unwrap();
    println!("{}", doc2.to_yaml_string());

    let yaml3 = r#"
Name: example-project
Repository: something
"#;
    println!("\nTest 3 - unquoted hyphen:");
    let parsed3 = Yaml::from_str(yaml3).unwrap();
    let doc3 = parsed3.document().unwrap();
    println!("{}", doc3.to_yaml_string());

    Ok(())
}
