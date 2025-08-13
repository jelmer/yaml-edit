use yaml_edit::ScalarValue;

fn main() {
    #[cfg(feature = "regex")]
    {
        println!("Testing regex extraction functionality...\n");
        
        // Example 1: Create regex scalars and extract compiled Regex objects
        let email_pattern = ScalarValue::regex(r"^[a-zA-Z0-9._%+-]+@[a-zA-Z0-9.-]+\.[a-zA-Z]{2,}$");
        let phone_pattern = ScalarValue::regex(r"^\+?1?\s*\(?(\d{3})\)?[\s\-\.]?(\d{3})[\s\-\.]?(\d{4})$");
        let zip_pattern = ScalarValue::regex(r"^\d{5}(-\d{4})?$");
        
        // Extract and use the compiled regex
        println!("Email validation:");
        if let Some(email_regex) = email_pattern.as_regex() {
            let test_emails = vec!["user@example.com", "invalid.email", "@no-user.com"];
            for email in test_emails {
                println!("  '{}' -> {}", email, 
                    if email_regex.is_match(email) { "✓ Valid" } else { "✗ Invalid" });
            }
        }
        
        println!("\nPhone validation:");
        if let Some(phone_regex) = phone_pattern.as_regex() {
            let test_phones = vec!["555-123-4567", "(555) 123-4567", "12345"];
            for phone in test_phones {
                println!("  '{}' -> {}", phone,
                    if phone_regex.is_match(phone) { "✓ Valid" } else { "✗ Invalid" });
            }
        }
        
        println!("\nZIP code validation:");
        if let Some(zip_regex) = zip_pattern.as_regex() {
            let test_zips = vec!["12345", "12345-6789", "1234"];
            for zip in test_zips {
                println!("  '{}' -> {}", zip,
                    if zip_regex.is_match(zip) { "✓ Valid" } else { "✗ Invalid" });
            }
        }
        
        // Example 2: Using try_as_regex() to compile plain strings as regex
        println!("\n\nUsing try_as_regex() for plain strings:");
        
        let version_pattern = ScalarValue::new(r"^v(\d+)\.(\d+)\.(\d+)$");
        match version_pattern.try_as_regex() {
            Ok(regex) => {
                println!("Version pattern compiled successfully");
                if let Some(captures) = regex.captures("v1.2.3") {
                    println!("  Parsed v1.2.3:");
                    println!("    Major: {}", captures.get(1).unwrap().as_str());
                    println!("    Minor: {}", captures.get(2).unwrap().as_str());
                    println!("    Patch: {}", captures.get(3).unwrap().as_str());
                }
            }
            Err(e) => println!("Failed to compile: {}", e),
        }
        
        // Example 3: Invalid regex handling
        println!("\nHandling invalid regex patterns:");
        let invalid = ScalarValue::regex(r"[unclosed");
        match invalid.as_regex() {
            Some(_) => println!("  Unexpectedly compiled invalid pattern"),
            None => println!("  ✓ Invalid pattern correctly rejected"),
        }
        
        println!("\n✅ Regex extraction test completed!");
    }
    
    #[cfg(not(feature = "regex"))]
    {
        println!("This example requires the 'regex' feature to be enabled.");
        println!("Run with: cargo run --example test_regex_extraction --features regex");
    }
}