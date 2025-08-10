fn main() {
    let input = "line with - and + and : characters";

    println!("Input: {}", input);
    println!("\nTokens:");

    // We'll have to test this manually since lex is not public
    // Let's just parse the YAML to see what happens
    use yaml_edit::Yaml;

    let parsed = Yaml::parse(input);
    let output = parsed.to_string();

    println!("Output: {}", output);
}
