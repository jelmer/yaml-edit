use yaml_edit::lex::{lex, SyntaxKind};

fn main() {
    let yaml = r#"items:
  - &first_item value1
  - second_item
  - *first_item"#;

    println!("Testing YAML:\n{}", yaml);

    let tokens = lex(yaml);

    println!("\nTokens:");
    for (i, (kind, text)) in tokens.iter().enumerate() {
        println!("  {}: {:?} = {:?}", i, kind, text);
    }
}
