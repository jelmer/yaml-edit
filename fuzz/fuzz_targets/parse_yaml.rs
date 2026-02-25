#![no_main]

use libfuzzer_sys::fuzz_target;
use yaml_edit::Parse;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, handling invalid UTF-8 gracefully
    if let Ok(yaml_str) = std::str::from_utf8(data) {
        // Skip extremely large inputs to avoid timeout
        if yaml_str.len() > 1_000_000 {
            return;
        }
        
        // Parse the YAML - should never panic
        let parse = Parse::parse_yaml(yaml_str);
        let _ = parse.tree();
        let _ = parse.errors();
    }
});
