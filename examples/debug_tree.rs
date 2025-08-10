use rowan::ast::AstNode;
use yaml_edit::{SyntaxNodeExt, Yaml};

fn print_tree_structure(node: &rowan::SyntaxNode<yaml_edit::Lang>, depth: usize) {
    let indent = "  ".repeat(depth);
    println!(
        "{}{:?}: '{}'",
        indent,
        node.kind(),
        node.text().to_string().replace('\n', "\\n")
    );

    for child in node.children() {
        print_tree_structure(&child, depth + 1);
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let test_content = r#"Repository: [url1, url2, url3]"#;

    println!("Testing with content: {}", test_content);

    let parsed = Yaml::parse(test_content);
    let doc = parsed.tree().documents().next().unwrap();

    println!("\nTree structure:");
    print_tree_structure(doc.syntax(), 0);

    println!("\n---\n");

    let test_content2 = r#"Repository:
  - url1
  - url2"#;

    println!("Testing with content: {}", test_content2);

    let parsed2 = Yaml::parse(test_content2);
    let doc2 = parsed2.tree().documents().next().unwrap();

    println!("\nTree structure:");
    print_tree_structure(doc2.syntax(), 0);

    Ok(())
}
