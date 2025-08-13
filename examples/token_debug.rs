use yaml_edit::lex_with_validation;

fn main() {
    let yaml = r#"
empty_string: ""
empty_after_colon:
another_key: value
"#;

    println!("Tokens for empty values test:");
    let (tokens, _) = lex_with_validation(yaml);

    for (i, (kind, text)) in tokens.iter().enumerate() {
        println!("{:2}: {:?} = {:?}", i, kind, text);
    }
}
