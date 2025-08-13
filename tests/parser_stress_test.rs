//! Stress tests for YAML parser to ensure robustness

use yaml_edit::{Parse, Yaml};

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

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("deep_value"));
}

#[test]
fn test_large_number_of_keys() {
    let mut yaml = String::new();

    for i in 0..1000 {
        yaml.push_str(&format!("key{}: value{}\n", i, i));
    }

    let parse = Parse::parse_yaml(&yaml);
    let doc = parse.tree();
    assert!(doc.to_string().contains("key0"));
    assert!(doc.to_string().contains("key999"));
}

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

    // Should parse the valid parts
    assert!(doc.to_string().contains("valid_key"));
    assert!(doc.to_string().contains("item1"));
    assert!(doc.to_string().contains("&test"));
}

#[test]
fn test_unicode_content() {
    let yaml = r#"
english: Hello World
chinese: 你好世界
arabic: مرحبا بالعالم
emoji: 😀🎉🌟
mixed: "English 中文 العربية 🌍"
"#;

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("Hello"));
    assert!(doc.to_string().contains("你好"));
    assert!(doc.to_string().contains("😀"));
}

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

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("database"));
    assert!(doc.to_string().contains("servers"));
    assert!(doc.to_string().contains("web1"));
    assert!(doc.to_string().contains("192.168.1.1"));
}

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

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("empty_string"));
    assert!(doc.to_string().contains("null"));
    assert!(doc.to_string().contains("~"));
}

#[test]
fn test_various_scalars() {
    let yaml = r#"
string: hello world
integer: 42
float: 3.14159
boolean_true: true
boolean_false: false
hex: 0xFF
octal: 0o755
binary: 0b1010
scientific: 6.02e23
"#;

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("hello"));
    assert!(doc.to_string().contains("42"));
    assert!(doc.to_string().contains("3.14"));
    assert!(doc.to_string().contains("true"));
    assert!(doc.to_string().contains("0xFF"));
}

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

    let doc = Yaml::parse(yaml).to_result().unwrap();
    let output = doc.to_string();
    assert!(output.contains("# Top level comment"));
    assert!(output.contains("# Inline comment"));
    assert!(output.contains("# Comment block"));
    assert!(output.contains("key1"));
    assert!(output.contains("value2"));
}

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

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("literal"));
    assert!(doc.to_string().contains("folded"));
    assert!(doc.to_string().contains("plain_multiline"));
}

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

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("simple_list"));
    assert!(doc.to_string().contains("nested_list"));
    assert!(doc.to_string().contains("alice"));
    assert!(doc.to_string().contains("admin"));
}

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

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("&defaults"));
    assert!(doc.to_string().contains("*defaults"));
    assert!(doc.to_string().contains("<<"));
    assert!(doc.to_string().contains("production"));
}

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

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("---"));
    assert!(doc.to_string().contains("doc1"));
    assert!(doc.to_string().contains("doc2"));
    assert!(doc.to_string().contains("doc3"));
}

#[test]
fn test_special_characters_in_strings() {
    let yaml = r#"
simple: "hello world"
quoted: 'single quotes'
url: "https://example.com"
"#;

    let doc = Yaml::parse(yaml).to_result().unwrap();
    assert!(doc.to_string().contains("simple"));
    assert!(doc.to_string().contains("quoted"));
    assert!(doc.to_string().contains("url"));
}
