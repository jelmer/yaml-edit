//! Stress tests for YAML parser to ensure robustness
//!
//! These tests verify parser behavior with:
//! - Deep nesting and large documents
//! - Complex nested structures
//! - Various scalar types and values
//! - Collections (flow and block)
//! - Anchors, aliases, and merge keys
//! - Comments preservation
//! - Unicode content
//! - Document markers and directives

use yaml_edit::{Parse, YamlFile};

// ========================================
// Deep Nesting and Large Documents
// ========================================

#[test]
fn test_deeply_nested_structures() {
    // Test very deep nesting
    let yaml = r#"
level1:
  level2:
    level3:
      level4:
        level5: deep_value
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed the deeply nested structure correctly via API
    let document = doc.document().unwrap();
    let root = document.as_mapping().unwrap();
    let level1 = root.get_mapping("level1").unwrap();
    let level2 = level1.get_mapping("level2").unwrap();
    let level3 = level2.get_mapping("level3").unwrap();
    let level4 = level3.get_mapping("level4").unwrap();
    assert_eq!(
        level4
            .get("level5")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "deep_value"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_large_number_of_keys() {
    let mut yaml = String::new();

    for i in 0..1000 {
        yaml.push_str(&format!("key{}: value{}\n", i, i));
    }

    let parse = Parse::parse_yaml(&yaml);
    let doc = parse.tree();

    // Verify we parsed the large mapping correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(mapping.len(), 1000);
    assert_eq!(
        mapping
            .get("key0")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value0"
    );
    assert_eq!(
        mapping
            .get("key999")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value999"
    );
    assert_eq!(
        mapping
            .get("key250")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value250"
    );
    assert_eq!(
        mapping
            .get("key500")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value500"
    );
    assert_eq!(
        mapping
            .get("key750")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value750"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Degraded Parsing and Mixed Content
// ========================================

#[test]
fn test_mixed_valid_and_invalid() {
    let yaml = r#"
# This is valid
valid_key: valid_value
valid_list:
  - item1
  - item2

# This has some issues but should still parse
anchor: &test
ref: *test
another: value
"#;

    let parse = Parse::parse_yaml(yaml);
    let doc = parse.tree();

    // Verify we parsed the valid parts correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("valid_key")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "valid_value"
    );

    let valid_list_node = mapping.get("valid_list").unwrap();
    let valid_list = valid_list_node.as_sequence().unwrap();
    assert_eq!(valid_list.len(), 2);
    assert_eq!(
        valid_list.get(0).unwrap().as_scalar().unwrap().as_string(),
        "item1"
    );
    assert_eq!(
        valid_list.get(1).unwrap().as_scalar().unwrap().as_string(),
        "item2"
    );

    // Verify anchor value (the anchor marker creates an empty/null scalar)
    assert!(mapping.get("anchor").unwrap().as_scalar().is_some());

    // Verify ref is an alias to "test"
    let ref_val = mapping.get("ref").unwrap();
    assert!(ref_val.is_alias());
    assert_eq!(ref_val.as_alias().unwrap().name(), "test");

    assert_eq!(
        mapping
            .get("another")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Unicode Support
// ========================================

#[test]
fn test_unicode_content() {
    let yaml = r#"
english: Hello World
chinese: 你好世界
arabic: مرحبا بالعالم
emoji: 😀🎉🌟
mixed: "English 中文 العربية 🌍"
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed the unicode content correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("english")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "Hello World"
    );
    assert_eq!(
        mapping
            .get("chinese")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "你好世界"
    );
    assert_eq!(
        mapping
            .get("arabic")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "مرحبا بالعالم"
    );
    assert_eq!(
        mapping
            .get("emoji")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "😀🎉🌟"
    );
    assert_eq!(
        mapping
            .get("mixed")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "English 中文 العربية 🌍"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Complex Nested Structures
// ========================================

#[test]
fn test_complex_structures() {
    let yaml = r#"
config:
  database:
    host: localhost
    port: 5432
    credentials:
      username: admin
      password: secret
  servers:
    - name: web1
      ip: 192.168.1.1
      services: [http, https]
    - name: web2
      ip: 192.168.1.2
      services: [http, https, ssh]
  features:
    caching: true
    logging:
      level: info
      file: /var/log/app.log
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed the complex structure correctly via API
    let document = doc.document().unwrap();
    let root = document.as_mapping().unwrap();
    let config = root.get_mapping("config").unwrap();

    // Verify database section
    let database = config.get_mapping("database").unwrap();
    assert_eq!(
        database
            .get("host")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "localhost"
    );
    assert_eq!(
        database
            .get("port")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "5432"
    );
    let credentials = database.get_mapping("credentials").unwrap();
    assert_eq!(
        credentials
            .get("username")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "admin"
    );

    // Verify servers section
    let servers = config.get_sequence("servers").unwrap();
    assert_eq!(servers.len(), 2);
    let web1_node = servers.get(0).unwrap();
    let web1 = web1_node.as_mapping().unwrap();
    assert_eq!(
        web1.get("name").unwrap().as_scalar().unwrap().as_string(),
        "web1"
    );
    assert_eq!(
        web1.get("ip").unwrap().as_scalar().unwrap().as_string(),
        "192.168.1.1"
    );
    let web1_services = web1.get_sequence("services").unwrap();
    assert_eq!(web1_services.len(), 2);

    // Verify features section
    let features = config.get_mapping("features").unwrap();
    assert_eq!(features.get("caching").unwrap().to_bool(), Some(true));
    let logging = features.get_mapping("logging").unwrap();
    assert_eq!(
        logging
            .get("level")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "info"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Scalar Types and Values
// ========================================

#[test]
fn test_empty_and_null_values() {
    let yaml = r#"
empty_string: ""
null_explicit: null
null_implicit:
tilde_null: ~
empty_list: []
empty_map: {}
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed empty and null values correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("empty_string")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        ""
    );
    assert!(mapping
        .get("null_explicit")
        .unwrap()
        .as_scalar()
        .unwrap()
        .is_null());
    assert!(mapping
        .get("null_implicit")
        .unwrap()
        .as_scalar()
        .unwrap()
        .is_null());
    assert!(mapping
        .get("tilde_null")
        .unwrap()
        .as_scalar()
        .unwrap()
        .is_null());

    let empty_list = mapping.get_sequence("empty_list").unwrap();
    assert_eq!(empty_list.len(), 0);

    let empty_map = mapping.get_mapping("empty_map").unwrap();
    assert_eq!(empty_map.len(), 0);

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_various_scalars() {
    let yaml = r#"
string: hello world
integer: 42
float: 3.125
boolean_true: true
boolean_false: false
hex: 0xFF
octal: 0o755
binary: 0b1010
scientific: 6.02e23
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed various scalar types correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("string")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "hello world"
    );
    assert_eq!(mapping.get("integer").unwrap().to_i64(), Some(42));
    assert_eq!(mapping.get("float").unwrap().to_f64(), Some(3.125));
    assert_eq!(mapping.get("boolean_true").unwrap().to_bool(), Some(true));
    assert_eq!(mapping.get("boolean_false").unwrap().to_bool(), Some(false));
    assert_eq!(mapping.get("hex").unwrap().to_i64(), Some(0xFF));
    assert_eq!(mapping.get("octal").unwrap().to_i64(), Some(0o755));
    assert_eq!(mapping.get("binary").unwrap().to_i64(), Some(0b1010));
    assert_eq!(mapping.get("scientific").unwrap().to_f64(), Some(6.02e23));

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Comments Preservation
// ========================================

#[test]
fn test_comments_preservation() {
    let yaml = r#"
# Top level comment
key1: value1  # Inline comment

# Comment block
key2: value2
    # Indented comment
key3: value3  # Another inline

# Final comment
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed the keys correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("key1")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value1"
    );
    assert_eq!(
        mapping
            .get("key2")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value2"
    );
    assert_eq!(
        mapping
            .get("key3")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value3"
    );

    // Verify exact round-trip (comments are preserved)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Multiline Scalars
// ========================================

#[test]
fn test_multiline_scalars() {
    let yaml = r#"
literal: |
  This is a literal
  multiline string
  with preserved newlines

folded: >
  This is a folded
  multiline string
  that will be folded
  into a single line

plain_multiline: this is a plain
  multiline scalar that
  continues on multiple lines
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed the multiline scalars correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    let literal = mapping
        .get("literal")
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(
        literal,
        "This is a literal\nmultiline string\nwith preserved newlines\n"
    );

    let folded = mapping
        .get("folded")
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(
        folded,
        "This is a folded multiline string that will be folded into a single line\n"
    );

    let plain_multiline = mapping
        .get("plain_multiline")
        .unwrap()
        .as_scalar()
        .unwrap()
        .as_string();
    assert_eq!(
        plain_multiline,
        "this is a plain multiline scalar that continues on multiple lines"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Flow Collections
// ========================================

#[test]
fn test_flow_collections() {
    let yaml = r#"
simple_list: [1, 2, 3, 4, 5]
nested_list: [[1, 2], [3, 4], [5, 6]]
simple_map: {a: 1, b: 2, c: 3}
mixed: [{name: alice, age: 30}, {name: bob, age: 25}]
complex: {
  users: [
    {name: user1, roles: [admin, user]},
    {name: user2, roles: [user]}
  ],
  settings: {debug: true, timeout: 30}
}
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed the flow collections correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    let simple_list = mapping.get_sequence("simple_list").unwrap();
    assert_eq!(simple_list.len(), 5);
    assert!(simple_list.is_flow_style());

    let nested_list = mapping.get_sequence("nested_list").unwrap();
    assert_eq!(nested_list.len(), 3);
    assert!(nested_list.is_flow_style());

    let simple_map = mapping.get_mapping("simple_map").unwrap();
    assert_eq!(simple_map.len(), 3);
    assert!(simple_map.is_flow_style());

    let mixed = mapping.get_sequence("mixed").unwrap();
    assert_eq!(mixed.len(), 2);
    let first_person_node = mixed.get(0).unwrap();
    let first_person = first_person_node.as_mapping().unwrap();
    assert_eq!(
        first_person
            .get("name")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "alice"
    );

    let complex = mapping.get_mapping("complex").unwrap();
    let users = complex.get_sequence("users").unwrap();
    let first_user_node = users.get(0).unwrap();
    let first_user = first_user_node.as_mapping().unwrap();
    let roles = first_user.get_sequence("roles").unwrap();
    assert_eq!(
        roles.get(0).unwrap().as_scalar().unwrap().as_string(),
        "admin"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Anchors and Aliases
// ========================================

#[test]
fn test_anchors_and_aliases() {
    let yaml = r#"
defaults: &defaults
  timeout: 30
  retries: 3

production:
  <<: *defaults
  host: prod.example.com

development:
  <<: *defaults
  host: dev.example.com
  debug: true

template: &template
  version: "1.0"

service1:
  <<: *template
  name: service1

service2:
  <<: *template
  name: service2
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed anchors and merge keys correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    // Verify anchored values
    let defaults = mapping.get_mapping("defaults").unwrap();
    assert_eq!(
        defaults
            .get("timeout")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "30"
    );

    let template = mapping.get_mapping("template").unwrap();
    assert_eq!(
        template
            .get("version")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "1.0"
    );

    // Verify mappings with merge keys
    let production = mapping.get_mapping("production").unwrap();
    assert_eq!(
        production
            .get("host")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "prod.example.com"
    );

    let development = mapping.get_mapping("development").unwrap();
    assert_eq!(
        development
            .get("host")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "dev.example.com"
    );
    assert_eq!(development.get("debug").unwrap().to_bool(), Some(true));

    let service1 = mapping.get_mapping("service1").unwrap();
    assert_eq!(
        service1
            .get("name")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "service1"
    );

    // Verify exact round-trip (preserves anchors, aliases, and merge keys)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Document Markers and Directives
// ========================================

#[test]
fn test_document_markers() {
    let yaml = r#"
%YAML 1.2
---
doc1: value1
...
---
doc2: value2
---
doc3: value3
...
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed the first document correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(
        mapping
            .get("doc1")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value1"
    );

    // Verify exact round-trip (preserves directives, document markers, and all documents)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Special Characters in Strings
// ========================================

#[test]
fn test_special_characters_in_strings() {
    let yaml = r#"
simple: "hello world"
quoted: 'single quotes'
url: "https://example.com"
"#;

    let doc = YamlFile::parse(yaml).to_result().unwrap();

    // Verify we parsed the special strings correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();

    assert_eq!(
        mapping
            .get("simple")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "hello world"
    );
    assert_eq!(
        mapping
            .get("quoted")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "single quotes"
    );
    assert_eq!(
        mapping.get("url").unwrap().as_scalar().unwrap().as_string(),
        "https://example.com"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

// ========================================
// Extreme Stress Tests
// ========================================

#[test]
fn test_very_large_mapping_10k_keys() {
    let mut yaml = String::new();

    for i in 0..10000 {
        yaml.push_str(&format!("key{}: value{}\n", i, i));
    }

    let parse = Parse::parse_yaml(&yaml);
    let doc = parse.tree();

    // Verify we parsed the large mapping correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(mapping.len(), 10000, "Should have 10000 keys");

    // Spot-check various keys throughout
    assert_eq!(
        mapping
            .get("key0")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value0"
    );
    assert_eq!(
        mapping
            .get("key2500")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value2500"
    );
    assert_eq!(
        mapping
            .get("key5000")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value5000"
    );
    assert_eq!(
        mapping
            .get("key7500")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value7500"
    );
    assert_eq!(
        mapping
            .get("key9999")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value9999"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_very_deep_nesting_100_levels() {
    // Build deeply nested structure: a: {b: {c: {...}}}
    let mut yaml = String::new();
    for i in 0..100 {
        yaml.push_str(&format!("{}key{}: {{\n", "  ".repeat(i), i));
    }
    yaml.push_str(&format!("{}value: deep\n", "  ".repeat(100)));
    for i in (0..100).rev() {
        yaml.push_str(&format!("{}}}\n", "  ".repeat(i)));
    }

    let parse = Parse::parse_yaml(&yaml);
    let doc = parse.tree();

    // Verify we parsed the deeply nested structure without crashing
    let document = doc.document().unwrap();
    let root = document.as_mapping().unwrap();

    // Navigate down a few levels to verify structure (checking first 10 levels)
    let mut next = root.get("key0").expect("Should have key0");
    for i in 1..10 {
        let key = format!("key{}", i);
        let mapping = next.as_mapping().expect("Should be mapping");
        next = mapping
            .get(&key)
            .unwrap_or_else(|| panic!("Should have {}", key));
    }

    // Verify we got deep enough
    let last_mapping = next.as_mapping().expect("Should be mapping at level 9");
    assert!(
        last_mapping.contains_key("key10"),
        "Should have key10 at level 10"
    );

    // Verify round-trip produces valid YAML
    let output = doc.to_string();
    let reparsed = Parse::parse_yaml(&output);
    assert!(reparsed.tree().document().is_some());
}

#[test]
fn test_very_long_sequence_10k_items() {
    let mut yaml = String::new();
    yaml.push_str("items:\n");

    for i in 0..10000 {
        yaml.push_str(&format!("  - item{}\n", i));
    }

    let parse = Parse::parse_yaml(&yaml);
    let doc = parse.tree();

    // Verify we parsed the long sequence correctly via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    let items = mapping.get_sequence("items").unwrap();

    assert_eq!(items.len(), 10000, "Should have 10000 items");

    // Spot-check various items
    assert_eq!(
        items.get(0).unwrap().as_scalar().unwrap().as_string(),
        "item0"
    );
    assert_eq!(
        items.get(2500).unwrap().as_scalar().unwrap().as_string(),
        "item2500"
    );
    assert_eq!(
        items.get(5000).unwrap().as_scalar().unwrap().as_string(),
        "item5000"
    );
    assert_eq!(
        items.get(9999).unwrap().as_scalar().unwrap().as_string(),
        "item9999"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_large_document_1mb() {
    // Create a document that's approximately 1MB
    // Each line is ~21 bytes: "key00000: value00000\n"
    // 1MB / 21 bytes ≈ 48,000 lines
    let mut yaml = String::new();

    for i in 0..48000 {
        yaml.push_str(&format!("key{:05}: value{:05}\n", i, i));
    }

    // Verify size is roughly 1MB
    assert!(yaml.len() > 900_000, "Should be close to 1MB");
    assert!(yaml.len() < 1_100_000, "Should be close to 1MB");

    let parse = Parse::parse_yaml(&yaml);
    let doc = parse.tree();

    // Verify we parsed the large document via API
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(mapping.len(), 48000, "Should have 48000 keys");

    // Spot-check a few keys
    assert_eq!(
        mapping
            .get("key00000")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value00000"
    );
    assert_eq!(
        mapping
            .get("key24000")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value24000"
    );
    assert_eq!(
        mapping
            .get("key47999")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value47999"
    );

    // Verify exact round-trip
    let output = doc.to_string();
    assert_eq!(output, yaml);
}

#[test]
fn test_many_comments_10k() {
    let mut yaml = String::new();

    for i in 0..10000 {
        yaml.push_str(&format!("# Comment {}\n", i));
        yaml.push_str(&format!("key{}: value{}\n", i, i));
    }

    let parse = Parse::parse_yaml(&yaml);
    let doc = parse.tree();

    // Verify we parsed correctly preserving comments
    let document = doc.document().unwrap();
    let mapping = document.as_mapping().unwrap();
    assert_eq!(mapping.len(), 10000, "Should have 10000 keys");

    // Spot-check keys
    assert_eq!(
        mapping
            .get("key0")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value0"
    );
    assert_eq!(
        mapping
            .get("key9999")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "value9999"
    );

    // Verify exact round-trip (comments are preserved)
    let output = doc.to_string();
    assert_eq!(output, yaml);
}
