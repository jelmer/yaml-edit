//! Parsing and round-tripping of valid input: large and deeply nested
//! documents, scalar styles, multi-key documents, byte order marks and JSON.

use std::str::FromStr;
use yaml_edit::{CoreScalarType, Document, Parse, ScalarValue, SyntaxKind};

mod common;
use common::{
    assert_cst_ok, assert_shapes, assert_strings, check_renders, keys, node_at, parse,
    render_documents,
};

#[test]
fn deeply_nested_structures() {
    let yaml = r#"
level1:
  level2:
    level3:
      level4:
        level5: deep_value
"#;
    let file = parse(yaml);
    let document = file.document().unwrap();
    let level4 = document
        .as_mapping()
        .unwrap()
        .get_mapping("level1")
        .unwrap()
        .get_mapping("level2")
        .unwrap()
        .get_mapping("level3")
        .unwrap()
        .get_mapping("level4")
        .unwrap();
    assert_eq!(
        level4
            .get("level5")
            .unwrap()
            .as_scalar()
            .unwrap()
            .as_string(),
        "deep_value"
    );
}

#[test]
fn large_block_mappings() {
    fn plain(i: usize) -> String {
        format!("key{}: value{}\n", i, i)
    }
    fn padded(i: usize) -> String {
        format!("key{:05}: value{:05}\n", i, i)
    }
    fn commented(i: usize) -> String {
        format!("# Comment {}\n", i) + &plain(i)
    }
    let mapping_of = |n: usize, line: fn(usize) -> String| (0..n).map(line).collect::<String>();

    let megabyte = mapping_of(48000, padded);
    assert!(megabyte.len() > 900_000, "{}", megabyte.len());
    assert!(megabyte.len() < 1_100_000, "{}", megabyte.len());

    for (yaml, len, spot_checks) in [
        (
            mapping_of(1000, plain),
            1000,
            &[
                ("key0", "value0"),
                ("key999", "value999"),
                ("key250", "value250"),
                ("key500", "value500"),
                ("key750", "value750"),
            ][..],
        ),
        (
            mapping_of(10000, plain),
            10000,
            &[
                ("key0", "value0"),
                ("key2500", "value2500"),
                ("key5000", "value5000"),
                ("key7500", "value7500"),
                ("key9999", "value9999"),
            ],
        ),
        (
            megabyte,
            48000,
            &[
                ("key00000", "value00000"),
                ("key24000", "value24000"),
                ("key47999", "value47999"),
            ],
        ),
        (
            mapping_of(10000, commented),
            10000,
            &[("key0", "value0"), ("key9999", "value9999")],
        ),
    ] {
        let file = Parse::parse_yaml(&yaml).tree();
        assert_eq!(file.to_string(), yaml);
        assert_eq!(file.document().unwrap().as_mapping().unwrap().len(), len);
        assert_strings(&file, spot_checks);
    }
}

#[test]
fn very_long_sequence() {
    let mut yaml = String::new();
    yaml.push_str("items:\n");
    for i in 0..10000 {
        yaml.push_str(&format!("  - item{}\n", i));
    }

    let file = Parse::parse_yaml(&yaml).tree();
    assert_eq!(file.to_string(), yaml);
    assert_shapes(&file, &[("items", "block sequence of 10000")]);
    assert_strings(
        &file,
        &[
            ("items.0", "item0"),
            ("items.2500", "item2500"),
            ("items.5000", "item5000"),
            ("items.9999", "item9999"),
        ],
    );
}

#[test]
fn very_deep_flow_nesting() {
    // key0: {key1: {... value: deep ...}} with each level on its own line.
    let mut yaml = String::new();
    for i in 0..100 {
        yaml.push_str(&format!("{}key{}: {{\n", "  ".repeat(i), i));
    }
    yaml.push_str(&format!("{}value: deep\n", "  ".repeat(100)));
    for i in (0..100).rev() {
        yaml.push_str(&format!("{}}}\n", "  ".repeat(i)));
    }

    let file = parse(&yaml);
    let path = (0..10).map(|i| format!("key{}", i)).collect::<Vec<_>>();
    let level9 = node_at(&file, &path.join("."));
    assert!(level9.as_mapping().unwrap().contains_key("key10"));
}

#[test]
fn mixed_content() {
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
    let file = parse(yaml);
    assert_strings(
        &file,
        &[
            ("valid_key", "valid_value"),
            ("valid_list.0", "item1"),
            ("valid_list.1", "item2"),
            ("another", "value"),
        ],
    );
    assert_shapes(&file, &[("valid_list", "block sequence of 2")]);
    // The anchor marks an empty scalar.
    assert!(node_at(&file, "anchor").as_scalar().is_some());
    let reference = node_at(&file, "ref");
    assert!(reference.is_alias());
    assert_eq!(reference.as_alias().unwrap().name(), "test");
}

#[test]
fn unicode_values() {
    for (yaml, expected) in [
        (
            r#"
english: Hello World
chinese: 你好世界
arabic: مرحبا بالعالم
emoji: 😀🎉🌟
mixed: "English 中文 العربية 🌍"
"#,
            &[
                ("english", "Hello World"),
                ("chinese", "你好世界"),
                ("arabic", "مرحبا بالعالم"),
                ("emoji", "😀🎉🌟"),
                ("mixed", "English 中文 العربية 🌍"),
            ][..],
        ),
        (
            r#"english: Hello
chinese: 你好
japanese: こんにちは
arabic: مرحبا
emoji: 😀🎉🚀
math: ∑∏∫√"#,
            &[
                ("english", "Hello"),
                ("chinese", "你好"),
                ("japanese", "こんにちは"),
                ("arabic", "مرحبا"),
                ("emoji", "😀🎉🚀"),
                ("math", "∑∏∫√"),
            ],
        ),
    ] {
        assert_strings(&parse(yaml), expected);
    }
}

#[test]
fn line_endings() {
    for yaml in [
        "key: value\r\nanother: value\r\nlist:\r\n  - item1\r\n  - item2",
        "key: value\ranother: value\rlist:\r  - item1\r  - item2",
        "key: value\nanother: value\r\nlist:\r  - item1\n  - item2",
    ] {
        let file = parse(yaml);
        assert_strings(
            &file,
            &[
                ("key", "value"),
                ("another", "value"),
                ("list.0", "item1"),
                ("list.1", "item2"),
            ],
        );
        assert_shapes(&file, &[("list", "block sequence of 2")]);
    }
}

#[test]
fn complex_structures() {
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
    let file = parse(yaml);
    assert_strings(
        &file,
        &[
            ("config.database.host", "localhost"),
            ("config.database.port", "5432"),
            ("config.database.credentials.username", "admin"),
            ("config.servers.0.name", "web1"),
            ("config.servers.0.ip", "192.168.1.1"),
            ("config.features.logging.level", "info"),
        ],
    );
    assert_shapes(
        &file,
        &[
            ("config.servers", "block sequence of 2"),
            ("config.servers.0.services", "flow sequence of 2"),
        ],
    );
    assert_eq!(
        node_at(&file, "config.features.caching").to_bool(),
        Some(true)
    );
}

#[test]
fn empty_and_null_values() {
    let yaml = r#"
empty_string: ""
null_explicit: null
null_implicit:
tilde_null: ~
empty_list: []
empty_map: {}
"#;
    let file = parse(yaml);
    assert_strings(&file, &[("empty_string", "")]);
    for key in ["null_explicit", "null_implicit", "tilde_null"] {
        assert!(node_at(&file, key).as_scalar().unwrap().is_null(), "{key}");
    }
    let mapping = file.document().unwrap().as_mapping().unwrap();
    assert_eq!(mapping.get_sequence("empty_list").unwrap().len(), 0);
    assert_eq!(mapping.get_mapping("empty_map").unwrap().len(), 0);
}

#[test]
fn various_scalars() {
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
    let file = parse(yaml);
    assert_strings(&file, &[("string", "hello world")]);
    let at = |key| node_at(&file, key);
    assert_eq!(at("integer").to_i64(), Some(42));
    assert_eq!(at("float").to_f64(), Some(3.125));
    assert_eq!(at("boolean_true").to_bool(), Some(true));
    assert_eq!(at("boolean_false").to_bool(), Some(false));
    assert_eq!(at("hex").to_i64(), Some(0xFF));
    assert_eq!(at("octal").to_i64(), Some(0o755));
    // YAML 1.2 has no binary integers.
    assert_eq!(at("binary").to_i64(), None);
    assert_eq!(at("scientific").to_f64(), Some(6.02e23));
}

#[test]
fn comments_between_entries() {
    let yaml = r#"
# Top level comment
key1: value1  # Inline comment

# Comment block
key2: value2
    # Indented comment
key3: value3  # Another inline

# Final comment
"#;
    assert_strings(
        &parse(yaml),
        &[("key1", "value1"), ("key2", "value2"), ("key3", "value3")],
    );
}

#[test]
fn multiline_scalars() {
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
    assert_strings(
        &parse(yaml),
        &[
            (
                "literal",
                "This is a literal\nmultiline string\nwith preserved newlines\n",
            ),
            (
                "folded",
                "This is a folded multiline string that will be folded into a single line\n",
            ),
            (
                "plain_multiline",
                "this is a plain multiline scalar that continues on multiple lines",
            ),
        ],
    );
}

#[test]
fn flow_collections() {
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
    let file = parse(yaml);
    assert_shapes(
        &file,
        &[
            ("simple_list", "flow sequence of 5"),
            ("nested_list", "flow sequence of 3"),
            ("simple_map", "flow mapping of 3"),
            ("mixed", "flow sequence of 2"),
        ],
    );
    assert_strings(
        &file,
        &[
            ("mixed.0.name", "alice"),
            ("complex.users.0.roles.0", "admin"),
        ],
    );
}

#[test]
fn anchors_and_merge_keys() {
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
    let file = parse(yaml);
    assert_strings(
        &file,
        &[
            ("defaults.timeout", "30"),
            ("template.version", "1.0"),
            ("production.host", "prod.example.com"),
            ("development.host", "dev.example.com"),
            ("service1.name", "service1"),
        ],
    );
    assert_eq!(node_at(&file, "development.debug").to_bool(), Some(true));
}

#[test]
fn quoted_strings() {
    let yaml = r#"
simple: "hello world"
quoted: 'single quotes'
url: "https://example.com"
"#;
    assert_strings(
        &parse(yaml),
        &[
            ("simple", "hello world"),
            ("quoted", "single quotes"),
            ("url", "https://example.com"),
        ],
    );
}

#[test]
fn round_trips() {
    for yaml in [
        "key: value",
        "\nname: John\nage: 30\nitems:\n  - apple\n  - banana\nconfig:\n  host: localhost\n  port: 8080\n",
        "key1 value: 1\nkey2: value\n",
    ] {
        parse(yaml);
    }
}

/// A plain scalar may contain spaces and tabs, also as a key (issue #30),
/// in block and flow context alike.
#[test]
fn multi_word_plain_scalars() {
    for (yaml, expected) in [
        ("abc cba: value\n", "map{abc cba: value}"),
        ("a b c d: value\n", "map{a b c d: value}"),
        ("abc   cba: value\n", "map{abc   cba: value}"),
        ("abc\tcba: value\n", "map{abc\tcba: value}"),
        // The key does not include the white space before the colon.
        ("abc cba : value\n", "map{abc cba: value}"),
        // Without a colon it is not a mapping.
        ("abc cba\n", "abc cba"),
        ("key: abc cba\n", "map{key: abc cba}"),
        (
            "key: abc cba\nkey2: value\n",
            "map{key: abc cba, key2: value}",
        ),
        ("key: value # comment\n", "map{key: value}"),
        ("abc cba: value # comment\n", "map{abc cba: value}"),
        ("key: abc cba # comment\n", "map{key: abc cba}"),
        // A `#` with no space before it is content.
        ("key: abc#def\n", "map{key: abc#def}"),
        (
            "http://example.com: value\n",
            "map{http://example.com: value}",
        ),
        ("- abc cba\n- foo\n", "seq[abc cba, foo]"),
        (
            "- abc cba: value\n- foo: bar\n",
            "seq[map{abc cba: value}, map{foo: bar}]",
        ),
        // In a sequence item, matching PyYAML.
        (
            "\n- item1: value1\n- item2 has text: but not a key\n- item3\n",
            "seq[map{item1: value1}, map{item2 has text: but not a key}, item3]",
        ),
        (
            "outer:\n  abc cba: 2\n  foo: 3\n",
            "map{outer: map{abc cba: 2, foo: 3}}",
        ),
        ("{abc cba: value}", "{abc cba: value}"),
        ("[abc cba, def ghi]", "[abc cba, def ghi]"),
        ("{a: b c d}", "{a: b c d}"),
        // `abc-` is a single word, and `cba` after the space joins it.
        ("abc- cba: value\n", "map{abc- cba: value}"),
        ("abc 123: value\n", "map{abc 123: value}"),
        ("true false: value\n", "map{true false: value}"),
        // A continuation line folds to a space.
        ("key: abc cba\n  more text\n", "map{key: abc cba more text}"),
        (
            "xyz: 1\nabc cba: 2\nfoo: 3\n",
            "map{xyz: 1, abc cba: 2, foo: 3}",
        ),
        (
            "key1 value: 1\nkey2: value\n",
            "map{key1 value: 1, key2: value}",
        ),
        (
            "# top comment\nouter:\n  # inner comment\n  abc cba: 1\n  foo: 2\n",
            "map{outer: map{abc cba: 1, foo: 2}}",
        ),
        ("abc cba: original\n", "map{abc cba: original}"),
    ] {
        parse(yaml);
        assert_eq!(render_documents(yaml), [expected], "{yaml:?}");
    }
}

/// A colon in a plain scalar starts no mapping entry unless a space
/// follows it, so URLs, times and ports stay whole values.
#[test]
fn colons_in_plain_scalars() {
    check_renders(&[
        (
            "timestamp: 2001-12-14 21:59:43.10 -5",
            "map{timestamp: 2001-12-14 21:59:43.10 -5}",
        ),
        (
            r#"
timestamps:
  simple: 2001-12-14
  with_time: 2001-12-14 21:59:43
  with_fractional: 2001-12-14 21:59:43.10
  with_timezone: 2001-12-14 21:59:43.10 -5
  with_utc: 2001-12-14 21:59:43.10 Z
"#,
            "map{timestamps: map{simple: 2001-12-14, with_time: 2001-12-14 21:59:43, \
             with_fractional: 2001-12-14 21:59:43.10, with_timezone: 2001-12-14 21:59:43.10 -5, \
             with_utc: 2001-12-14 21:59:43.10 Z}}",
        ),
        // `: ` in a plain value is a spec violation the validator reports;
        // the parser keeps the rest of the line as the value.
        (
            r#"
url: http://example.com:8080
time: "12:30:45"
description: This is a value: with a colon
"#,
            "map{url: http://example.com:8080, time: 12:30:45, \
             description: This is a value: with a colon}",
        ),
        (
            r#"
http_url: http://example.com:8080/path?query=value
https_url: https://secure.example.com:443/secure/path
ftp_url: ftp://files.example.com:21/directory/file.txt
file_url: file:///path/to/local/file.txt
ssh_url: ssh://user@example.com:22/path
"#,
            "map{http_url: http://example.com:8080/path?query=value, \
             https_url: https://secure.example.com:443/secure/path, \
             ftp_url: ftp://files.example.com:21/directory/file.txt, \
             file_url: file:///path/to/local/file.txt, ssh_url: ssh://user@example.com:22/path}",
        ),
        (
            r#"
database:
  host: db.example.com
  port: 5432
  url: postgresql://user:password@db.example.com:5432/database
web:
  api_url: https://api.example.com:443/v1
  timeout: 30
"#,
            "map{database: map{host: db.example.com, port: 5432, \
             url: postgresql://user:password@db.example.com:5432/database}, \
             web: map{api_url: https://api.example.com:443/v1, timeout: 30}}",
        ),
        (
            r#"
server: example.com:8080
time_24h: 14:30:45
time_12h: 2:30:45 PM
timestamp: 2023-12-25 14:30:45.123 -05:00
ipv4: 192.168.1.1:8080
ipv6_bracket: "[::1]:8080"
ratio: 3:2:1
"#,
            "map{server: example.com:8080, time_24h: 14:30:45, time_12h: 2:30:45 PM, \
             timestamp: 2023-12-25 14:30:45.123 -05:00, ipv4: 192.168.1.1:8080, \
             ipv6_bracket: [::1]:8080, ratio: 3:2:1}",
        ),
        (
            r#"
services:
  - name: web
    url: http://web.example.com:80
    endpoints:
      health: http://web.example.com:80/health
      api: http://web.example.com:80/api/v1
  - name: database
    url: postgresql://db:5432/app
    config:
      timeout: "30s"
      retries: 3
urls:
  - https://api.github.com/repos/owner/repo
  - ftp://files.example.com/downloads
  - ssh://git@github.com:owner/repo.git
"#,
            "map{services: seq[map{name: web, url: http://web.example.com:80, \
             endpoints: map{health: http://web.example.com:80/health, \
             api: http://web.example.com:80/api/v1}}, map{name: database, \
             url: postgresql://db:5432/app, config: map{timeout: 30s, retries: 3}}], \
             urls: seq[https://api.github.com/repos/owner/repo, ftp://files.example.com/downloads, \
             ssh://git@github.com:owner/repo.git]}",
        ),
        (
            r#"
# URLs at different positions
start_url: http://start.com
middle: some text with http://embedded.com:8080 url
end_with_url: ends with http://end.com:9000

# URLs with special characters (limited by YAML constraints)
basic_auth: https://user:pass@example.com:443
with_fragment: http://example.com:8080/path#section
with_query: http://example.com:8080/path?param=value

# Non-URLs that contain colons
not_url_1: namespace:function_name
# Note: "key: value" without quotes is invalid YAML (colon+space in plain scalar)
# Must use quotes for this to be valid
not_url_2: "key: value on same line"
not_url_3: "quoted:string:with:colons"

# Edge case: colon at end
colon_end: "ends with:"
"#,
            "map{start_url: http://start.com, middle: some text with http://embedded.com:8080 url, \
             end_with_url: ends with http://end.com:9000, \
             basic_auth: https://user:pass@example.com:443, \
             with_fragment: http://example.com:8080/path#section, \
             with_query: http://example.com:8080/path?param=value, \
             not_url_1: namespace:function_name, not_url_2: key: value on same line, \
             not_url_3: quoted:string:with:colons, colon_end: ends with:}",
        ),
    ]);

    // A URL is a single token.
    let (tokens, _) = yaml_edit::lex_with_validation("url: https://example.com:443/path");
    assert_eq!(
        tokens,
        [
            (SyntaxKind::STRING, "url"),
            (SyntaxKind::COLON, ":"),
            (SyntaxKind::WHITESPACE, " "),
            (SyntaxKind::STRING, "https://example.com:443/path"),
        ]
    );
}

#[test]
fn mixed_block_and_flow_styles() {
    check_renders(&[(
        "flow: [a, b, c]\nblock:\n  - item1\n  - item2\nnested: {key: value}\n",
        "map{flow: [a, b, c], block: seq[item1, item2], nested: {key: value}}",
    )]);
}

/// In flow context a plain scalar can contain spaces too, so
/// `{host localhost}` is a key with an implicit null value (as PyYAML has it).
#[test]
fn flow_mapping_with_implicit_null_values() {
    let file = parse(r#"config: {host localhost, port 8080}"#);
    let config = file.document().unwrap().get_mapping("config").unwrap();
    let keys: Vec<String> = config.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, ["host localhost", "port 8080"]);
}

/// Write `yaml` to a file and load it with [`Document::from_file`].
fn document_from_file(name: &str, yaml: &str) -> Document {
    let path = std::env::temp_dir().join(format!("yaml-edit-{}-{name}", std::process::id()));
    std::fs::write(&path, yaml).unwrap();
    let doc = Document::from_file(&path);
    std::fs::remove_file(&path).unwrap();
    doc.unwrap()
}

#[test]
fn multi_key_documents() {
    for (name, yaml, expected) in [
        (
            "multi_key.yaml",
            r#"---
Repository: https://github.com/example/blah.git
Repository-Browse: https://github.com/example/blah
Security-Contact: https://github.com/example/blah/tree/HEAD/SECURITY.md
"#,
            &[
                ("Repository", "https://github.com/example/blah.git"),
                ("Repository-Browse", "https://github.com/example/blah"),
                (
                    "Security-Contact",
                    "https://github.com/example/blah/tree/HEAD/SECURITY.md",
                ),
            ][..],
        ),
        (
            "single_key.yaml",
            r#"---
Repository: https://github.com/example/blah.git
"#,
            &[("Repository", "https://github.com/example/blah.git")],
        ),
        // Colons in values must not start a new key.
        (
            "multiple_urls.yaml",
            r#"---
homepage: https://example.com:8080/path
documentation: http://docs.example.com/guide
api: https://api.example.com:443/v1
download: ftp://ftp.example.com:21/files/latest.tar.gz
"#,
            &[
                ("homepage", "https://example.com:8080/path"),
                ("documentation", "http://docs.example.com/guide"),
                ("api", "https://api.example.com:443/v1"),
                ("download", "ftp://ftp.example.com:21/files/latest.tar.gz"),
            ],
        ),
        (
            "colons_contexts.yaml",
            r#"---
time: "10:30:45"
ratio: 16:9
path: /usr/local/bin:/usr/bin:/bin
windows_path: C:\Users\Example\Documents
equation: "y = 2x + 3: where x > 0"
"#,
            &[
                ("time", "10:30:45"),
                ("ratio", "16:9"),
                ("path", "/usr/local/bin:/usr/bin:/bin"),
                ("windows_path", "C:\\Users\\Example\\Documents"),
                ("equation", "y = 2x + 3: where x > 0"),
            ],
        ),
    ] {
        let doc = document_from_file(name, yaml);
        let keys: Vec<String> = doc.keys().map(|k| k.to_string()).collect();
        let expected_keys: Vec<&str> = expected.iter().map(|(k, _)| *k).collect();
        assert_eq!(keys, expected_keys);
        for (key, value) in expected {
            assert_eq!(doc.get_string(key).unwrap(), *value, "{key}");
        }
    }
}

#[test]
fn parse_then_edit_multikey_document() {
    let yaml_content = r#"---
first: https://first.example.com
second: https://second.example.com
third: https://third.example.com
"#;
    let doc = parse(yaml_content).documents().next().unwrap();
    assert_eq!(doc.keys().count(), 3);

    doc.set("second", "https://updated.example.com");
    assert_cst_ok(&doc);
    assert_eq!(
        doc.get_string("first").unwrap(),
        "https://first.example.com"
    );
    assert_eq!(
        doc.get_string("second").unwrap(),
        "https://updated.example.com"
    );
    assert_eq!(
        doc.get_string("third").unwrap(),
        "https://third.example.com"
    );

    doc.set("fourth", "https://fourth.example.com");
    assert_cst_ok(&doc);
    assert_eq!(doc.keys().count(), 4);
    assert_eq!(
        doc.get_string("fourth").unwrap(),
        "https://fourth.example.com"
    );
}

/// A UTF-8 BOM opening the stream is kept in the text but is not part of
/// the first key.
#[test]
fn bom_at_start() {
    for (yaml, expected) in [
        ("\u{FEFF}key: value", &[("key", "value")][..]),
        ("key: value", &[("key", "value")]),
        ("\u{FEFF}---\nkey: value", &[("key", "value")]),
        // A comment may follow the BOM directly (l-document-prefix).
        ("\u{FEFF}# c\na: 1\n", &[("a", "1")]),
        // The BOM is not content, so a node may start right after it.
        ("\u{FEFF}? a\n: 1\n", &[("a", "1")]),
    ] {
        let file = parse(yaml);
        let expected_keys: Vec<&str> = expected.iter().map(|(k, _)| *k).collect();
        assert_eq!(keys(&file), expected_keys, "{yaml:?}");
        assert_strings(&file, expected);
    }

    let file = parse("\u{FEFF}&x [a]\n");
    assert_eq!(file.document().unwrap().as_sequence().unwrap().len(), 1);
    let file = parse("\u{FEFF}!!map {k: v}\n");
    assert_eq!(keys(&file), vec!["k"]);
}

/// The BOM is not part of the first line, so a body indented one space
/// past a column-0 entry is still indented past it.
#[test]
fn bom_takes_no_column() {
    let file = parse("\u{FEFF}- |\n x\n- y\n");
    assert_shapes(&file, &[("", "block sequence of 2")]);
    assert_strings(&file, &[("0", "x\n"), ("1", "y")]);

    let file = parse("\u{FEFF}key: &anchor\n !!map\n  a: b\n");
    assert_eq!(keys(&file), vec!["key"]);
}

/// A BOM in the middle of a document is part of the key that follows.
#[test]
fn bom_between_lines() {
    let bom_in_middle = "key1: value1\n\u{FEFF}key2: value2";
    let file = parse(bom_in_middle);
    assert_eq!(keys(&file), ["key1", "\u{FEFF}key2"]);
}

/// A BOM may open any document in the stream, not just the first.
#[test]
fn bom_at_start_of_later_document() {
    for yaml in ["a: 1\n---\n\u{FEFF}b: 2\n", "a: 1\n\u{FEFF}---\nb: 2\n"] {
        let file = parse(yaml);
        let docs: Vec<_> = file.documents().collect();
        assert_eq!(docs.len(), 2, "{yaml:?}");
        let keys: Vec<String> = docs[1]
            .as_mapping()
            .unwrap()
            .keys()
            .map(|k| k.to_string())
            .collect();
        assert_eq!(keys, vec!["b"], "{yaml:?}");
    }
}

#[test]
fn json_object() {
    let json = r#"{"name": "John", "age": 30, "active": true, "balance": 100.50}"#;
    let file = parse(json);
    assert_strings(&file, &[("name", "John")]);
    for (key, value) in [("age", "30"), ("active", "true"), ("balance", "100.50")] {
        assert_eq!(node_at(&file, key).as_scalar().unwrap().value(), value);
    }
}

#[test]
fn json_array() {
    let json = r#"[1, 2, 3, "hello", true, null]"#;
    let doc = parse(json).document().unwrap();
    let items: Vec<String> = doc
        .as_sequence()
        .unwrap()
        .values()
        .map(|item| item.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(items, vec!["1", "2", "3", "hello", "true", "null"]);
}

#[test]
fn nested_json() {
    let json = r#"{
        "users": [
            {"name": "Alice", "age": 25},
            {"name": "Bob", "age": 30}
        ],
        "settings": {
            "theme": "dark",
            "notifications": true
        }
    }"#;
    let file = parse(json);
    assert_shapes(
        &file,
        &[
            ("users", "flow sequence of 2"),
            ("settings", "flow mapping of 2"),
        ],
    );
    assert_strings(&file, &[("settings.theme", "dark")]);
}

/// JSON escapes become the characters they stand for.
#[test]
fn json_escape_sequences() {
    let json =
        r#"{"message": "Hello\nWorld\t!", "path": "C:\\Users\\file.txt", "unicode": "\u2764"}"#;
    assert_strings(
        &parse(json),
        &[
            ("message", "Hello\nWorld\t!"),
            ("path", "C:\\Users\\file.txt"),
            ("unicode", "\u{2764}"),
        ],
    );
}

#[test]
fn json_numbers() {
    let json = r#"{
        "integer": 42,
        "negative": -17,
        "float": 3.14159,
        "scientific": 6.022e23,
        "negative_exp": 1.5e-10
    }"#;
    let file = parse(json);
    for (key, value) in [
        ("integer", "42"),
        ("negative", "-17"),
        ("float", "3.14159"),
        ("scientific", "6.022e23"),
        ("negative_exp", "1.5e-10"),
    ] {
        assert_eq!(node_at(&file, key).as_scalar().unwrap().value(), value);
    }
}

#[test]
fn json_empty_values() {
    let json = r#"{"empty_string": "", "empty_array": [], "empty_object": {}}"#;
    let file = parse(json);
    assert_strings(&file, &[("empty_string", "")]);
    assert_shapes(
        &file,
        &[
            ("empty_array", "flow sequence of 0"),
            ("empty_object", "flow mapping of 0"),
        ],
    );
}

#[test]
fn json_compatible_subset() {
    let yaml = r#"{
  "string": "value",
  "number": 42,
  "float": 3.25,
  "boolean": true,
  "null": null,
  "array": [1, 2, 3],
  "object": {"nested": "value"}
}"#;
    let file = parse(yaml);
    assert_strings(&file, &[("string", "value"), ("object.nested", "value")]);
    assert_eq!(node_at(&file, "number").to_i64(), Some(42));
    assert_eq!(node_at(&file, "float").to_f64(), Some(3.25));
    assert_eq!(node_at(&file, "boolean").to_bool(), Some(true));
    assert!(node_at(&file, "null").as_scalar().unwrap().is_null());
    assert_shapes(&file, &[("array", "flow sequence of 3")]);
}

#[test]
fn json_round_trips() {
    for json in [
        r#"{"name": "test", "items": [1, 2, 3]}"#,
        // JSON allows arbitrary whitespace between tokens, also at the
        // end of a line.
        "  {  \n        \"key1\"   :   \"value1\"  ,\n        \"key2\"  :  [  1  ,  2  ,  3  ]  \n    }  ",
        r#"{"a": 1, "b": 2}"#,
        r#"[1, 2, 3]"#,
    ] {
        parse(json);
    }
}

#[test]
fn malformed_legacy_octal_is_an_integer_not_a_float() {
    // `08` is not valid octal, so parse_integer gives up by contract and
    // the value fell through to the float parser. It is a run of digits:
    // saphyr reads 8 and PyYAML the string, but no YAML reads a float.
    assert_eq!(ScalarValue::classify_plain("08"), CoreScalarType::Integer);
    assert_eq!(ScalarValue::classify_plain("099"), CoreScalarType::Integer);
    assert_eq!(ScalarValue::classify_plain("0755"), CoreScalarType::Integer);
}

#[test]
fn bare_octal_reads_as_decimal() {
    // YAML 1.2 dropped the bare-octal form, so a leading zero is decimal.
    let scalar = ScalarValue::parse("0755");
    assert_eq!(scalar.to_i64(), Some(755));
    #[allow(deprecated)]
    let compat = scalar.to_i64_yaml_1_2();
    assert_eq!(compat, Some(755));
    assert_eq!(ScalarValue::parse("-0755").to_i64(), Some(-755));

    let doc = Document::from_str("mode: 0755\n").unwrap();
    let mode = doc.as_mapping().unwrap().get("mode").unwrap();
    assert_eq!(mode.as_scalar().unwrap().as_i64(), Some(755));
    assert_eq!(mode.to_i64(), Some(755));
}

#[test]
fn typed_accessors_follow_core_schema() {
    // Forms outside the YAML 1.2 core schema are strings, and every typed
    // accessor agrees on that.
    for text in ["+0x1F", "-0o7", "0b101", "0X1F", "0O7", "-0x1F", "+0o7"] {
        assert_eq!(
            ScalarValue::classify_plain(text),
            CoreScalarType::String,
            "{text}"
        );
        let doc = Document::from_str(&format!("k: {text}\n")).unwrap();
        let node = doc.as_mapping().unwrap().get("k").unwrap();
        assert_eq!(node.as_scalar().unwrap().as_i64(), None, "{text}");
        assert_eq!(node.as_scalar().unwrap().as_f64(), None, "{text}");
        assert_eq!(node.to_i64(), None, "{text}");
        assert_eq!(ScalarValue::parse(text).to_i64(), None, "{text}");
    }

    for text in ["yes", "No", "on", "OFF", "tRUE"] {
        assert_eq!(
            ScalarValue::classify_plain(text),
            CoreScalarType::String,
            "{text}"
        );
        let doc = Document::from_str(&format!("k: {text}\n")).unwrap();
        let node = doc.as_mapping().unwrap().get("k").unwrap();
        assert_eq!(node.as_scalar().unwrap().as_bool(), None, "{text}");
        assert_eq!(node.to_bool(), None, "{text}");
        assert_eq!(ScalarValue::parse(text).to_bool(), None, "{text}");
    }

    for (text, expected) in [
        (".inf", f64::INFINITY),
        ("-.Inf", f64::NEG_INFINITY),
        ("+.INF", f64::INFINITY),
        ("1.", 1.0),
        (".5", 0.5),
        ("1e3", 1000.0),
        ("-2.5E-1", -0.25),
    ] {
        assert_eq!(
            ScalarValue::classify_plain(text),
            CoreScalarType::Float,
            "{text}"
        );
        assert_eq!(ScalarValue::parse(text).to_f64(), Some(expected), "{text}");
        let doc = Document::from_str(&format!("k: {text}\n")).unwrap();
        let node = doc.as_mapping().unwrap().get("k").unwrap();
        assert_eq!(node.to_f64(), Some(expected), "{text}");
        assert_eq!(node.as_scalar().unwrap().as_f64(), Some(expected), "{text}");
    }
    assert!(ScalarValue::parse(".NaN").to_f64().unwrap().is_nan());

    for text in [
        "inf", "nan", "Infinity", "1_000", "1e", "e5", ".", "+.", "1.5.2",
    ] {
        assert_eq!(
            ScalarValue::classify_plain(text),
            CoreScalarType::String,
            "{text}"
        );
        assert_eq!(ScalarValue::parse(text).to_f64(), None, "{text}");
    }
}

#[test]
fn non_breaking_space_is_not_yaml_whitespace() {
    // YAML whitespace is space and tab only; U+00A0 is ordinary content.
    use SyntaxKind::*;
    let doc = Document::from_str("a:\u{a0}b\n").unwrap();
    assert_eq!(doc.as_scalar().unwrap().as_string(), "a:\u{a0}b");

    let doc = Document::from_str("a\u{a0}#b\n").unwrap();
    assert_eq!(doc.as_scalar().unwrap().as_string(), "a\u{a0}#b");

    let doc = Document::from_str("-\u{a0}x\n").unwrap();
    assert_eq!(doc.as_scalar().unwrap().as_string(), "-\u{a0}x");

    assert_eq!(
        yaml_edit::lex("&a\u{a0}b c\n"),
        vec![
            (ANCHOR, "&a\u{a0}b"),
            (WHITESPACE, " "),
            (STRING, "c"),
            (NEWLINE, "\n"),
        ]
    );
}
