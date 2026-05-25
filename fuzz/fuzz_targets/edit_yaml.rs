#![no_main]

use libfuzzer_sys::fuzz_target;
use std::str::FromStr;
use yaml_edit::Document;

fuzz_target!(|data: &[u8]| {
    if let Ok(yaml_str) = std::str::from_utf8(data) {
        if yaml_str.len() > 100_000 {
            return;
        }

        if let Ok(doc) = Document::from_str(yaml_str) {
            if let Some(mapping) = doc.as_mapping() {
                mapping.set("fuzz_key", "fuzz_value");

                let _ = mapping.get("fuzz_key");

                let _ = mapping.contains_key("fuzz_key");
                let _ = mapping.len();
                let _ = mapping.is_empty();

                for key in mapping.keys() {
                    let _ = key.to_string();
                }

                for (key, _value) in mapping.iter() {
                    let _ = key.to_string();
                }
            }

            if let Some(seq) = doc.as_sequence() {
                seq.push("fuzz_item");

                let _ = seq.len();
                let _ = seq.is_empty();
                let _ = seq.first();
                let _ = seq.last();

                for _item in seq.values() {
                    // Just iterate
                }
            }

            let _ = doc.to_string();
        }
    }
});
