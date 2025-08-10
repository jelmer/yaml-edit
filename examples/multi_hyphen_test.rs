use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml = r#"
Name: example-project
Repository: something-else
"#;

    println!("Testing multi-line with hyphens: {}", yaml);
    let parsed = Yaml::from_str(yaml)?;
    let doc = parsed.document().unwrap();

    println!("Result: {}", doc.to_yaml_string());
    println!("Keys: {:?}", doc.keys());

    Ok(())
}
