#![no_main]

use libfuzzer_sys::fuzz_target;
use yaml_edit::YamlFile;
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, handling invalid UTF-8 gracefully
    if let Ok(yaml_str) = std::str::from_utf8(data) {
        // Skip extremely large inputs to avoid timeout
        if yaml_str.len() > 100_000 {
            return;
        }

        // Try to parse multi-document YAML - should never panic
        if let Ok(yaml) = YamlFile::from_str(yaml_str) {
            // Try to iterate documents
            for doc in yaml.documents() {
                // Try to access as mapping
                if let Some(mapping) = doc.as_mapping() {
                    mapping.set("fuzz", "value");
                }

                // Try to serialize
                let _ = doc.to_string();
            }

            // Try to access directives
            for directive in yaml.directives() {
                let _ = directive.name();
                let _ = directive.value();
            }

            // Try to serialize entire file
            let _ = yaml.to_string();
        }
    }
});
