use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    let yaml_content = "spaced: 2001-12-14 21:59:43.10 -5";

    eprintln!("Testing: {}", yaml_content);

    match Yaml::from_str(yaml_content) {
        Ok(yaml) => {
            println!("✅ Success");
            println!("Output: {}", yaml.to_string());
        }
        Err(e) => {
            println!("❌ Error: {}", e);
        }
    }
}
