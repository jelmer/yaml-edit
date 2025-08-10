use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Test 1: Just Name
    let yaml1 = r#"
Name: example-project
"#;
    println!("Test 1 - Name only:");
    let parsed1 = Yaml::from_str(yaml1).unwrap();
    let doc1 = parsed1.document().unwrap();
    println!("{}", doc1.to_yaml_string());
    println!("Keys: {:?}", doc1.keys());

    // Test 2: Name and one more line
    let yaml2 = r#"
Name: example-project
Repository: something
"#;
    println!("\nTest 2 - Name + Repository:");
    let parsed2 = Yaml::from_str(yaml2).unwrap();
    let doc2 = parsed2.document().unwrap();
    println!("{}", doc2.to_yaml_string());
    println!("Keys: {:?}", doc2.keys());

    // Test 3: Different name format
    let yaml3 = r#"
name: example-project
repository: something
"#;
    println!("\nTest 3 - lowercase keys:");
    let parsed3 = Yaml::from_str(yaml3).unwrap();
    let doc3 = parsed3.document().unwrap();
    println!("{}", doc3.to_yaml_string());
    println!("Keys: {:?}", doc3.keys());

    Ok(())
}
