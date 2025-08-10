fn main() {
    // Simulating the actual lexer behavior
    let input = "timestamp: 2024-01-15T10:30:00-05:00";

    println!("Input: {}", input);

    // The timestamp value starts after "timestamp: "
    let value_part = "2024-01-15T10:30:00-05:00";
    println!("Value part: {}", value_part);

    // With our current logic, it should be one token since hyphens in scalars are included
    // But let's trace through the logic

    // Starting with '2', we're in the default case
    // We read until we hit whitespace or a YAML special character
    // Hyphens ARE YAML special, so we'd stop at the first hyphen

    println!("\nHow lexer would tokenize the value:");
    println!("  Token 1: '2024' (stops at hyphen)");
    println!("  Token 2: '-' (hyphen is special)");
    println!("  Token 3: '01' (continues)");
    println!("  Token 4: '-' (another hyphen)");
    println!("  etc.");

    println!("\nBut wait - in the hyphen case, if it's not a sequence marker,");
    println!("we read the rest as a scalar INCLUDING hyphens!");
    println!("So the first hyphen would trigger reading '-01-15T10:30:00-05:00'");
}
