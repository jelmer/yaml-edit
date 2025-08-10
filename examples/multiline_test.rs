use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml_content = r#"literal: |
  This is a literal scalar
  that preserves line breaks
  and trailing whitespace  

folded: >
  This is a folded scalar
  that should fold lines
  into a single line

plain: |
  line 1
  line 2
  line 3"#;

    println!("Original YAML:");
    println!("{}", yaml_content);

    // Parse the YAML
    match Yaml::from_str(yaml_content) {
        Ok(yaml) => {
            println!("\nParsed and formatted YAML:");
            println!("{}", yaml);
        }
        Err(e) => {
            println!("\nParsing error: {}", e);
        }
    }

    Ok(())
}
