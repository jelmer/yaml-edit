use std::str::FromStr;

fn main() {
    let yaml_content = r#"
name: yaml-edit
version: 0.1.0
dependencies:
  - rowan
  - regex
features:
  lossless: true
"#;

    match yaml_edit::Yaml::from_str(yaml_content) {
        Ok(yaml) => {
            println!("Successfully parsed YAML:");
            println!("{}", yaml);
        }
        Err(e) => {
            println!("Failed to parse YAML: {}", e);
        }
    }
}
