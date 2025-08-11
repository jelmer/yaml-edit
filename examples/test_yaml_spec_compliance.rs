use std::str::FromStr;
use yaml_edit::Yaml;

fn main() {
    println!("Testing YAML 1.2 Specification Compliance\n");
    println!("==========================================\n");

    // Test 1: Plain scalars with special characters
    test_feature(
        "Plain scalars with special characters",
        r#"
# These should all be valid plain scalars
url: http://example.com/path
email: user@example.com
time: 12:34:56
date: 2024-01-15
negative: -42
positive: +42
decimal: 3.14159
scientific: 6.02e23
octal: 0o755
hex: 0xDEADBEEF
"#,
    );

    // Test 2: Complex keys
    test_feature(
        "Complex keys (explicit key indicator)",
        r#"
? - key1
  - key2
: value
"#,
    );

    test_feature(
        "Complex keys (mapping as key)",
        r#"
? name: John
  age: 30
: employee
"#,
    );

    // Test 3: Non-specific tags
    test_feature(
        "Non-specific tags",
        r#"
plain: !  value
quoted: ! "value"
"#,
    );

    // Test 4: Reserved directives
    test_feature(
        "Reserved directives",
        r#"
%RESERVED directive
---
key: value
"#,
    );

    // Test 5: Empty values
    test_feature(
        "Empty values in various contexts",
        r#"
empty_plain:
empty_quoted: ""
empty_single: ''
empty_block: |

empty_folded: >

empty_in_sequence:
  -
  - value
  -
empty_in_flow: [, value, ]
"#,
    );

    // Test 6: Implicit typing
    test_feature(
        "Implicit typing edge cases",
        r#"
# Should be strings, not booleans
yes_string: "yes"
no_string: 'no'
on_string: "on"
off_string: 'off'

# Should be strings, not null
null_string: "null"
tilde_string: "~"

# Octal and hex numbers
octal: 0o777
hex: 0xCAFE
binary: 0b101010

# Special floats
infinity: .inf
neg_infinity: -.inf
not_a_number: .nan
"#,
    );

    // Test 7: Flow collection edge cases
    test_feature(
        "Flow collection edge cases",
        r#"
# Empty flow collections
empty_seq: []
empty_map: {}

# Nested empty
nested: [[], {}, [{}], {a: []}]

# Single element
single_seq: [item]
single_map: {key: value}

# Trailing commas (should be valid)
trailing_seq: [a, b, c,]
trailing_map: {a: 1, b: 2,}
"#,
    );

    // Test 8: Indentation edge cases
    test_feature(
        "Indentation edge cases",
        r#"
# Mixed indentation levels
root:
  level1:
    level2:
     level2_off: value
      level3:
       level3_off: value
        level4: value
"#,
    );

    // Test 9: Anchor/alias edge cases
    test_feature(
        "Anchor/alias edge cases",
        r#"
# Anchor on different types
scalar_anchor: &scalar test
seq_anchor: &seq [1, 2, 3]
map_anchor: &map {a: 1, b: 2}

# Using them
use_scalar: *scalar
use_seq: *seq
use_map: *map

# Merge with multiple sources
merged:
  <<: [*map, *map]
  c: 3
"#,
    );

    // Test 10: Block scalar edge cases
    test_feature(
        "Block scalar edge cases",
        r#"
# Empty line handling
literal_empty_lines: |
  line1
  
  line3
  
  
  line6

# Leading empty lines
literal_leading: |
  
  first non-empty

# Explicit indentation with empty lines
explicit_indent: |2
  
    indented
  
    more
"#,
    );

    // Test 11: Special collection types
    test_feature(
        "Special collection types",
        r#"
# Set (unique values only)
set: !!set
  ? item1
  ? item2
  ? item1  # duplicate, should be ignored

# Ordered map
omap: !!omap
  - key1: value1
  - key2: value2
  - key3: value3

# Pairs
pairs: !!pairs
  - key1: value1
  - key2: value2
"#,
    );

    // Test 12: Binary data
    test_feature(
        "Binary data",
        r#"
# Binary data in base64
binary: !!binary |
  R0lGODlhDAAMAIQAAP//9/X17unp5WZmZgAAAOfn515eXvPz7Y6OjuDg4J+fn5
  OTk6enp56enmlpaWNjY6Ojo4SEhP/++f/++f/++f/++f/++f/++f/++f/++f/+
  +f/++f/++f/++f/++f/++SH+Dk1hZGUgd2l0aCBHSU1QACwAAAAADAAMAAAFLC
  AgjoEwnuNAFOhpEMTRiggcz4BNJHrv/zCFcLiwMWYNG84BwwEeECcgggoBADs=
"#,
    );

    // Test 13: Timestamps
    test_feature(
        "Timestamps",
        r#"
# ISO 8601 timestamps
canonical: 2001-12-15T02:59:43.1Z
iso8601: 2001-12-14t21:59:43.10-05:00
spaced: 2001-12-14 21:59:43.10 -5
date: 2002-12-14
"#,
    );

    // Test 14: Explicit typing with tags
    test_feature(
        "Explicit typing with tags",
        r#"
# Force string interpretation
string_true: !!str true
string_null: !!str null
string_number: !!str 123

# Force other types
int_string: !!int "123"
float_string: !!float "3.14"
bool_string: !!bool "yes"
"#,
    );

    // Test 15: Unicode and special characters
    test_feature(
        "Unicode and special characters",
        r#"
# Unicode in plain scalars
emoji: 😀🎉🚀
chinese: 你好世界
arabic: مرحبا بالعالم
math: ∑∫∂∇

# Special characters that need escaping
special: "Line1\nLine2\tTabbed\rCarriage\0Null"
unicode_escape: "\u263A\U0001F600"
"#,
    );

    // Test 16: Circular references
    test_feature(
        "Circular references (should error)",
        r#"
# This should produce an error
node: &node
  next: *node
"#,
    );

    // Test 17: Multiple document edge cases
    test_feature(
        "Multiple documents edge cases",
        r#"
# Empty document
---
...
---
# Document with just a scalar
just a string
---
# Normal document
key: value
...
"#,
    );

    // Test 18: Mixed flow and block
    test_feature(
        "Mixed flow and block styles",
        r#"
mixed:
  block_seq:
    - item1
    - [nested, flow, seq]
    - item3
  flow_in_block: {a: 1, b: [2, 3], c: {d: 4}}
  block_in_flow: [
    item1,
    {
      nested: value,
      another: value
    },
    item3
  ]
"#,
    );

    println!("\n==========================================");
    println!("Test Complete - Review output for issues");
}

fn test_feature(name: &str, yaml: &str) {
    println!("Testing: {}", name);
    println!(
        "Input: {}",
        if yaml.len() > 100 {
            format!("{}...", &yaml[..100])
        } else {
            yaml.to_string()
        }
    );

    match Yaml::from_str(yaml) {
        Ok(doc) => {
            let output = doc.to_string();
            let input_lines = yaml.lines().count();
            let output_lines = output.lines().count();

            if yaml.trim() == output.trim() {
                println!("✅ PERFECT: Exact preservation");
            } else if input_lines == output_lines {
                println!("✓ GOOD: Parsed successfully, same line count");
            } else {
                println!(
                    "⚠ WARNING: Parsed but output differs ({} lines -> {} lines)",
                    input_lines, output_lines
                );
            }
        }
        Err(e) => {
            println!("❌ FAILED: {:?}", e);
        }
    }
    println!();
}
