use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml = r#"items:
  - &first_item value1
  - second_item
  - *first_item"#;

    println!("Testing YAML:\n{}", yaml);

    let parsed = Yaml::from_str(yaml)?;
    let output = parsed.to_string();

    println!("\nParsed output:\n{}", output);
    println!("\nContains &first_item: {}", output.contains("&first_item"));

    Ok(())
}
