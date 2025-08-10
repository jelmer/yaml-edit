use std::str::FromStr;
use yaml_edit::Yaml;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let yaml_content = r#"# Test different types of tags
local_tag: !custom value
global_tag: !!str hello
builtin_int: !!int 123
builtin_bool: !!bool true
non_specific: ! bare_value
non_specific_global: !! another_value
quoted_tagged: !tag "quoted string""#;

    println!("Original YAML:");
    println!("{}", yaml_content);

    // Parse the YAML
    match Yaml::from_str(yaml_content) {
        Ok(yaml) => {
            println!("\nParsed and formatted YAML:");
            println!("{}", yaml);

            // Check the actual values
            if let Some(doc) = yaml.document() {
                println!("\nActual values:");

                let test_keys = [
                    "local_tag",
                    "global_tag",
                    "builtin_int",
                    "builtin_bool",
                    "non_specific",
                    "non_specific_global",
                    "quoted_tagged",
                ];

                for key in &test_keys {
                    if let Some(value) = doc.get_string(key) {
                        println!("{}: {:?}", key, value);
                    }
                }
            }
        }
        Err(e) => {
            println!("\nParsing error: {}", e);
        }
    }

    Ok(())
}
