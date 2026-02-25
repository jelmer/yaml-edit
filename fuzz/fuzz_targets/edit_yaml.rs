#![no_main]

use libfuzzer_sys::fuzz_target;
use yaml_edit::Document;
use std::str::FromStr;

fuzz_target!(|data: &[u8]| {
    // Convert bytes to string, handling invalid UTF-8 gracefully
    if let Ok(yaml_str) = std::str::from_utf8(data) {
        // Skip extremely large inputs to avoid timeout
        if yaml_str.len() > 100_000 {
            return;
        }

        // Try to parse and edit the YAML - should never panic
        if let Ok(doc) = Document::from_str(yaml_str) {
            // Try various edit operations
            if let Some(mapping) = doc.as_mapping() {
                // Try to set a value
                mapping.set("fuzz_key", "fuzz_value");

                // Try to get a value
                let _ = mapping.get("fuzz_key");

                // Try to remove a value
                mapping.remove("fuzz_key");

                // Try to iterate
                for (key, _value) in mapping.iter() {
                    let _ = key.as_string();
                }
            }

            if let Some(seq) = doc.as_sequence() {
                // Try to push a value
                seq.push("fuzz_item");

                // Try to iterate
                for _item in seq.iter() {
                    // Just iterate
                }
            }

            // Try to serialize - should never panic
            let _ = doc.to_string();
        }
    }
});
