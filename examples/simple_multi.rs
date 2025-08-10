use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml = r#"
array: [1, 2, 3]
object: {key: value, another: 42}
"#;

    let parsed = Yaml::from_str(yaml).unwrap();
    let doc = parsed.document().unwrap();

    println!("Parsed:");
    println!("{}", doc.to_yaml_string());
    println!("Keys: {:?}", doc.keys());

    Ok(())
}
