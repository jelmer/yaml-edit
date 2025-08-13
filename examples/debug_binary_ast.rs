use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    let binary_yaml = r#"binary: !!binary |
  SGVsbG8gV29ybGQ="#;

    println!("Testing binary AST:");

    match Yaml::from_str(binary_yaml) {
        Ok(yaml) => {
            println!("AST structure:");
            println!("{:#?}", yaml.syntax());
        }
        Err(e) => {
            println!("❌ Parse error: {}", e);
        }
    }
}
