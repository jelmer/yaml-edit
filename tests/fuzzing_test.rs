//! Fuzzing tests for YAML parser
//!
//! These tests generate random inputs to ensure the parser doesn't panic
//! and handles arbitrary input gracefully.

use std::panic;
use yaml_edit::Parse;

/// Generate random YAML-like content
fn generate_random_yaml(seed: usize, complexity: usize) -> String {
    let mut rng = SimpleRng::new(seed);
    let mut yaml = String::new();

    for _ in 0..complexity {
        match rng.next() % 10 {
            0 => yaml.push_str(&format!(
                "key{}: value{}\n",
                rng.next() % 100,
                rng.next() % 100
            )),
            1 => yaml.push_str(&format!("  - item{}\n", rng.next() % 100)),
            2 => yaml.push_str(&format!("&anchor{} data\n", rng.next() % 10)),
            3 => yaml.push_str(&format!("*alias{}\n", rng.next() % 10)),
            4 => yaml.push_str("# comment\n"),
            5 => yaml.push_str("[1, 2, 3]\n"),
            6 => yaml.push_str("{a: b, c: d}\n"),
            7 => yaml.push_str("---\n"),
            8 => yaml.push_str("...\n"),
            _ => yaml.push_str(&format!("{}\n", rng.next() % 1000)),
        }

        // Add some chaos
        if rng.next() % 5 == 0 {
            yaml.push_str(&"  ".repeat(rng.next() % 5));
        }
        if rng.next() % 10 == 0 {
            yaml.push('\n');
        }
    }

    yaml
}

/// Generate completely random bytes (not necessarily valid UTF-8)
fn generate_random_bytes(seed: usize, size: usize) -> Vec<u8> {
    let mut rng = SimpleRng::new(seed);
    let mut bytes = Vec::with_capacity(size);

    for _ in 0..size {
        bytes.push((rng.next() % 256) as u8);
    }

    bytes
}

/// Simple pseudo-random number generator for reproducible tests
struct SimpleRng {
    state: usize,
}

impl SimpleRng {
    fn new(seed: usize) -> Self {
        SimpleRng { state: seed }
    }

    fn next(&mut self) -> usize {
        self.state = self.state.wrapping_mul(1103515245).wrapping_add(12345);
        (self.state / 65536) % 32768
    }
}

#[test]
fn test_fuzz_random_yaml() {
    // Test with various random YAML-like inputs
    for seed in 0..100 {
        for complexity in &[10, 50, 100] {
            let yaml = generate_random_yaml(seed, *complexity);

            // Should not panic
            let result = panic::catch_unwind(|| {
                let parse = Parse::parse_yaml(&yaml);
                let _ = parse.tree();
                let _ = parse.errors();
            });

            assert!(
                result.is_ok(),
                "Parser panicked on seed {}, complexity {}",
                seed,
                complexity
            );
        }
    }
}

#[test]
fn test_fuzz_random_unicode() {
    // Test with random unicode characters
    let unicode_ranges = vec![
        (0x0000, 0x007F),   // ASCII
        (0x0080, 0x00FF),   // Latin-1 Supplement
        (0x0100, 0x017F),   // Latin Extended-A
        (0x0400, 0x04FF),   // Cyrillic
        (0x4E00, 0x9FFF),   // CJK Unified Ideographs
        (0x1F600, 0x1F64F), // Emoticons
    ];

    let mut rng = SimpleRng::new(42);

    for _ in 0..50 {
        let mut yaml = String::new();

        for _ in 0..100 {
            let range = &unicode_ranges[rng.next() % unicode_ranges.len()];
            let codepoint = range.0 + (rng.next() % (range.1 - range.0));

            if let Some(ch) = char::from_u32(codepoint as u32) {
                yaml.push(ch);
            }

            if rng.next() % 10 == 0 {
                yaml.push('\n');
            }
        }

        // Should not panic
        let result = panic::catch_unwind(|| {
            let parse = Parse::parse_yaml(&yaml);
            let _ = parse.tree();
        });

        assert!(result.is_ok(), "Parser panicked on unicode input");
    }
}

#[test]
fn test_fuzz_nested_structures() {
    let mut rng = SimpleRng::new(123);

    for depth in 1..20 {
        let mut yaml = String::new();
        let mut indent = 0;

        // Generate deeply nested structure
        for _ in 0..depth {
            yaml.push_str(&"  ".repeat(indent));

            match rng.next() % 3 {
                0 => {
                    yaml.push_str(&format!("key{}:\n", rng.next() % 10));
                    indent += 1;
                }
                1 => {
                    yaml.push_str(&format!("- item{}\n", rng.next() % 10));
                }
                _ => {
                    yaml.push_str(&format!("[{}, {}]\n", rng.next() % 10, rng.next() % 10));
                }
            }
        }

        // Add some values at the deepest level
        yaml.push_str(&"  ".repeat(indent));
        yaml.push_str("final: value\n");

        // Should not panic
        let result = panic::catch_unwind(|| {
            let parse = Parse::parse_yaml(&yaml);
            let _ = parse.tree();
        });

        assert!(
            result.is_ok(),
            "Parser panicked on nested structure depth {}",
            depth
        );
    }
}

#[test]
fn test_fuzz_special_characters() {
    let special_chars = ['!', '@', '#', '$', '%', '^', '&', '*', '(', ')', '-', '_', '=', '+', '[', ']', '{', '}',
        '|', '\\', ':', ';', '"', '\'', '<', '>', ',', '.', '?', '/', '~', '`'];

    let mut rng = SimpleRng::new(456);

    for _ in 0..100 {
        let mut yaml = String::new();

        for _ in 0..50 {
            // Mix special characters with normal text
            if rng.next() % 2 == 0 {
                yaml.push(special_chars[rng.next() % special_chars.len()]);
            } else {
                yaml.push_str(&format!("text{}", rng.next() % 100));
            }

            if rng.next() % 5 == 0 {
                yaml.push(' ');
            }
            if rng.next() % 10 == 0 {
                yaml.push('\n');
            }
        }

        // Should not panic
        let result = panic::catch_unwind(|| {
            let parse = Parse::parse_yaml(&yaml);
            let _ = parse.tree();
        });

        assert!(result.is_ok(), "Parser panicked on special characters");
    }
}

#[test]
fn test_fuzz_block_scalars() {
    let mut rng = SimpleRng::new(789);

    for _ in 0..50 {
        let mut yaml = String::new();

        yaml.push_str("key: ");

        // Random block scalar indicator
        match rng.next() % 2 {
            0 => yaml.push('|'),
            _ => yaml.push('>'),
        }

        // Random indentation indicator
        if rng.next() % 2 == 0 {
            yaml.push_str(&format!("{}", 1 + rng.next() % 9));
        }

        // Random chomping indicator
        match rng.next() % 3 {
            0 => yaml.push('+'),
            1 => yaml.push('-'),
            _ => {}
        }

        yaml.push('\n');

        // Random content with various indentations
        for _ in 0..10 {
            yaml.push_str(&" ".repeat(rng.next() % 10));
            yaml.push_str(&format!("line {}\n", rng.next() % 100));
        }

        // Should not panic
        let result = panic::catch_unwind(|| {
            let parse = Parse::parse_yaml(&yaml);
            let _ = parse.tree();
        });

        assert!(result.is_ok(), "Parser panicked on block scalar");
    }
}

#[test]
fn test_fuzz_flow_collections() {
    let mut rng = SimpleRng::new(321);

    for _ in 0..100 {
        let mut yaml = String::new();

        // Generate random flow collection
        let depth = 1 + rng.next() % 5;
        let mut stack = Vec::new();

        for _ in 0..depth {
            if rng.next() % 2 == 0 {
                yaml.push('[');
                stack.push(']');
            } else {
                yaml.push('{');
                stack.push('}');
            }

            // Add some content
            yaml.push_str(&format!("{}", rng.next() % 100));

            if rng.next() % 2 == 0 {
                yaml.push_str(", ");
            }
        }

        // Close brackets (sometimes incorrectly for testing)
        while let Some(closer) = stack.pop() {
            if rng.next() % 10 != 0 {
                // 90% chance of correct closing
                yaml.push(closer);
            }
        }

        // Should not panic
        let result = panic::catch_unwind(|| {
            let parse = Parse::parse_yaml(&yaml);
            let _ = parse.tree();
        });

        assert!(result.is_ok(), "Parser panicked on flow collection");
    }
}

#[test]
fn test_fuzz_comments_and_directives() {
    let mut rng = SimpleRng::new(654);

    for _ in 0..50 {
        let mut yaml = String::new();

        // Add random directives
        if rng.next() % 2 == 0 {
            yaml.push_str("%YAML 1.2\n");
        }
        if rng.next() % 2 == 0 {
            yaml.push_str(&format!("%TAG ! tag:example.com,{}:\n", rng.next() % 2025));
        }

        yaml.push_str("---\n");

        // Mix content with comments
        for _ in 0..20 {
            if rng.next() % 3 == 0 {
                yaml.push_str(&format!("# comment {}\n", rng.next() % 100));
            } else {
                yaml.push_str(&format!(
                    "key{}: value{} # inline comment\n",
                    rng.next() % 10,
                    rng.next() % 10
                ));
            }
        }

        if rng.next() % 2 == 0 {
            yaml.push_str("...\n");
        }

        // Should not panic
        let result = panic::catch_unwind(|| {
            let parse = Parse::parse_yaml(&yaml);
            let _ = parse.tree();
        });

        assert!(result.is_ok(), "Parser panicked on comments/directives");
    }
}

#[test]
fn test_fuzz_extreme_sizes() {
    // Test with very large inputs
    let sizes = vec![1000, 10000, 100000];

    for size in sizes {
        // Large key
        let yaml1 = format!("{}: value", "x".repeat(size));

        // Large value
        let yaml2 = format!("key: {}", "x".repeat(size));

        // Many keys
        let mut yaml3 = String::new();
        for i in 0..size.min(1000) {
            yaml3.push_str(&format!("key{}: value{}\n", i, i));
        }

        for yaml in &[yaml1, yaml2, yaml3] {
            // Should not panic
            let result = panic::catch_unwind(|| {
                let parse = Parse::parse_yaml(yaml);
                let _ = parse.tree();
            });

            assert!(
                result.is_ok(),
                "Parser panicked on large input size {}",
                size
            );
        }
    }
}

#[test]
fn test_fuzz_anchors_and_aliases() {
    let mut rng = SimpleRng::new(987);

    for _ in 0..50 {
        let mut yaml = String::new();
        let num_anchors = rng.next() % 10;

        // Define some anchors
        for i in 0..num_anchors {
            yaml.push_str(&format!("anchor{}: &a{} value{}\n", i, i, i));
        }

        // Reference anchors (some valid, some invalid)
        for _ in 0..20 {
            let anchor_id = rng.next() % (num_anchors + 5); // Some will be invalid
            yaml.push_str(&format!("ref: *a{}\n", anchor_id));
        }

        // Circular references
        if rng.next() % 2 == 0 {
            yaml.push_str("circular: &circ\n  ref: *circ\n");
        }

        // Should not panic
        let result = panic::catch_unwind(|| {
            let parse = Parse::parse_yaml(&yaml);
            let _ = parse.tree();
        });

        assert!(result.is_ok(), "Parser panicked on anchors/aliases");
    }
}

#[test]
fn test_fuzz_invalid_utf8_recovery() {
    // Test that parser handles invalid UTF-8 sequences gracefully
    // Note: Rust strings must be valid UTF-8, so we test with edge cases

    let edge_cases = vec![
        "\u{FFFD}", // Replacement character
        "\u{0000}", // Null
        "\u{FEFF}", // BOM
        "\u{200B}", // Zero-width space
        "\u{2028}", // Line separator
        "\u{2029}", // Paragraph separator
    ];

    for edge in edge_cases {
        let yaml = format!("key: {}value", edge);

        // Should not panic
        let result = panic::catch_unwind(|| {
            let parse = Parse::parse_yaml(&yaml);
            let _ = parse.tree();
        });

        assert!(result.is_ok(), "Parser panicked on edge case UTF-8");
    }
}
