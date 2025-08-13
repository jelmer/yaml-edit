use yaml_edit::lex_with_validation;

fn main() {
    let yaml = r#"---
Repository: https://github.com/example/blah.git
Repository-Browse: https://github.com/example/blah
Security-Contact: https://github.com/example/blah/tree/HEAD/SECURITY.md
"#;

    println!("Tokens for multi-key parsing test:");
    let (tokens, _) = lex_with_validation(yaml);

    for (i, (kind, text)) in tokens.iter().enumerate() {
        println!("{:2}: {:?} = {:?}", i, kind, text);
    }
}
