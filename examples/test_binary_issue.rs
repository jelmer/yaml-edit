use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    let binary_yaml = r#"binary: !!binary |
  SGVsbG8gV29ybGQ="#;

    println!("Testing binary data serialization:");
    println!("Input:\n{}", binary_yaml);
    println!("Input length: {}", binary_yaml.len());

    match Yaml::from_str(binary_yaml) {
        Ok(yaml) => {
            let output = yaml.to_string();
            println!("\n✅ Parsed successfully");
            println!("Output:\n{}", output);
            println!("Output length: {}", output.len());

            if output.len() < binary_yaml.len() {
                println!(
                    "❌ Content was lost! Output is {} characters shorter",
                    binary_yaml.len() - output.len()
                );
            } else {
                println!("✅ Content preserved");
            }
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }
}
