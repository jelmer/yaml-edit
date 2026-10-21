//! Examples from the YAML 1.2 specification, by section.
//! Reference: <https://yaml.org/spec/1.2.2/>
//!
//! Each input must parse without errors, round-trip byte for byte and give
//! the expected values through the API.

use rowan::ast::AstNode;
use yaml_edit::{Mapping, TaggedNode, YamlFile, YamlNode};

/// Parse `yaml`, requiring no errors and a byte-identical round trip.
#[track_caller]
fn parse(yaml: &str) -> YamlFile {
    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors(), Vec::<String>::new(), "{yaml:?}");
    let file = parsed.tree();
    assert_eq!(file.to_string(), yaml);
    file
}

/// The node at `path` in the first document: mapping keys and sequence
/// indices separated by `.`, or the root node for an empty path.
#[track_caller]
fn node_at(file: &YamlFile, path: &str) -> YamlNode {
    let doc = file.document().expect("no document");
    let root = doc
        .syntax()
        .children()
        .find_map(YamlNode::from_syntax)
        .expect("empty document");
    path.split('.')
        .filter(|segment| !segment.is_empty())
        .fold(root, |node, segment| {
            let child = if node.is_mapping() {
                node.get(segment)
            } else {
                node.get_item(segment.parse().expect("sequence index"))
            };
            child.unwrap_or_else(|| panic!("nothing at {segment:?} of {path:?}"))
        })
}

/// Check the string value of the scalar at each path.
#[track_caller]
fn assert_strings(file: &YamlFile, expected: &[(&str, &str)]) {
    for (path, value) in expected {
        let node = node_at(file, path);
        let scalar = node
            .as_scalar()
            .unwrap_or_else(|| panic!("{path:?} is not a scalar"));
        assert_eq!(scalar.as_string(), *value, "at {path:?}");
    }
}

/// Describe a collection as e.g. "flow sequence of 3".
fn shape(node: &YamlNode) -> String {
    let style = |flow: bool| if flow { "flow" } else { "block" };
    if let Some(seq) = node.as_sequence() {
        format!("{} sequence of {}", style(seq.is_flow_style()), seq.len())
    } else if let Some(map) = node.as_mapping() {
        format!("{} mapping of {}", style(map.is_flow_style()), map.len())
    } else {
        panic!("not a collection: {node:?}")
    }
}

#[track_caller]
fn assert_shapes(file: &YamlFile, expected: &[(&str, &str)]) {
    for (path, value) in expected {
        assert_eq!(shape(&node_at(file, path)), *value, "at {path:?}");
    }
}

/// Check that the node at each path is an alias of the given anchor.
#[track_caller]
fn assert_aliases(file: &YamlFile, expected: &[(&str, &str)]) {
    for (path, name) in expected {
        let node = node_at(file, path);
        let alias = node
            .as_alias()
            .unwrap_or_else(|| panic!("{path:?} is not an alias"));
        assert_eq!(alias.name(), *name, "at {path:?}");
    }
}

/// The keys of the first document's root mapping.
fn keys(file: &YamlFile) -> Vec<String> {
    let doc = file.document().unwrap();
    let mapping = doc.as_mapping().unwrap();
    mapping.keys().map(|k| k.to_string()).collect()
}

/// The value of the `<<` entry of `mapping`, found by iterating since `<<`
/// is a MERGE_KEY token rather than a STRING.
fn merge_value(mapping: &Mapping) -> YamlNode {
    mapping
        .iter()
        .find(|(k, _)| k.as_scalar().is_some_and(|s| s.as_string() == "<<"))
        .expect("no merge key")
        .1
}

// Section 2.1: Collections

#[test]
fn block_sequence() {
    let file = parse(
        r#"# Shopping list
- milk
- bread
- eggs"#,
    );
    assert_shapes(&file, &[("", "block sequence of 3")]);
    assert_strings(&file, &[("0", "milk"), ("1", "bread"), ("2", "eggs")]);
}

#[test]
fn flow_sequence() {
    let file = parse(r#"items: [apple, banana, orange]"#);
    assert_shapes(&file, &[("items", "flow sequence of 3")]);
    assert_strings(
        &file,
        &[
            ("items.0", "apple"),
            ("items.1", "banana"),
            ("items.2", "orange"),
        ],
    );
}

#[test]
fn block_mapping() {
    let file = parse(
        r#"name: John Doe
age: 30
city: New York"#,
    );
    assert_shapes(&file, &[("", "block mapping of 3")]);
    assert_strings(
        &file,
        &[("name", "John Doe"), ("age", "30"), ("city", "New York")],
    );
}

#[test]
fn flow_mapping() {
    let file = parse(r#"person: {name: Alice, age: 25, city: Boston}"#);
    assert_shapes(&file, &[("person", "flow mapping of 3")]);
    assert_strings(
        &file,
        &[
            ("person.name", "Alice"),
            ("person.age", "25"),
            ("person.city", "Boston"),
        ],
    );
}

#[test]
fn collections_nested_in_mapping() {
    for (yaml, name, shapes, strings) in [
        (
            r#"user:
  name: Alice
  skills:
    - Python
    - JavaScript"#,
            "Alice",
            ("user.skills", "block sequence of 2"),
            &[("user.skills.0", "Python"), ("user.skills.1", "JavaScript")],
        ),
        (
            r#"user:
  name: Alice
  projects:
    web: active
    mobile: pending"#,
            "Alice",
            ("user.projects", "block mapping of 2"),
            &[
                ("user.projects.web", "active"),
                ("user.projects.mobile", "pending"),
            ],
        ),
        (
            r#"user:
  name: Bob
  skills: [Java, C++]"#,
            "Bob",
            ("user.skills", "flow sequence of 2"),
            &[("user.skills.0", "Java"), ("user.skills.1", "C++")],
        ),
        (
            r#"user:
  name: Bob
  projects: {backend: done, frontend: active}"#,
            "Bob",
            ("user.projects", "flow mapping of 2"),
            &[
                ("user.projects.backend", "done"),
                ("user.projects.frontend", "active"),
            ],
        ),
    ] {
        let file = parse(yaml);
        assert_strings(&file, &[("user.name", name)]);
        assert_shapes(&file, &[shapes]);
        assert_strings(&file, strings);
    }
}

#[test]
fn mixed_block_and_flow_nesting() {
    let file = parse(
        r#"users:
  - name: Alice
    skills: [Python, JavaScript]
  - name: Bob
    projects: {backend: done}"#,
    );
    assert_shapes(
        &file,
        &[
            ("users", "block sequence of 2"),
            ("users.0.skills", "flow sequence of 2"),
            ("users.1.projects", "flow mapping of 1"),
        ],
    );
    assert_strings(&file, &[("users.0.name", "Alice"), ("users.1.name", "Bob")]);
}

// Section 2.2: Scalar styles

#[test]
fn plain_scalars() {
    let file = parse(
        r#"string: hello world
integer: 42
float: 3.14
boolean_true: true
boolean_false: false
null_value: null"#,
    );
    assert_strings(
        &file,
        &[
            ("string", "hello world"),
            ("integer", "42"),
            ("float", "3.14"),
            ("boolean_true", "true"),
            ("boolean_false", "false"),
            ("null_value", "null"),
        ],
    );
}

#[test]
fn single_quoted_scalars() {
    let file = parse(
        r#"single: 'This is a single-quoted string'
with_quotes: 'It''s got an apostrophe'
multiline: 'First line
  second line'"#,
    );
    assert_strings(
        &file,
        &[
            ("single", "This is a single-quoted string"),
            ("with_quotes", "It's got an apostrophe"),
            ("multiline", "First line second line"),
        ],
    );
}

#[test]
fn double_quoted_scalars() {
    let file = parse(
        r#"double: "This is a double-quoted string"
escaped: "Line 1\nLine 2\tTabbed"
unicode: "Smiley: \u263A"
quote: "She said \"Hello\"""#,
    );
    assert_strings(
        &file,
        &[
            ("double", "This is a double-quoted string"),
            ("escaped", "Line 1\nLine 2\tTabbed"),
            ("unicode", "Smiley: \u{263A}"),
            ("quote", "She said \"Hello\""),
        ],
    );
}

#[test]
fn literal_scalar() {
    let file = parse(
        r#"literal: |
  This is a literal scalar.
  It preserves newlines.

  And blank lines too."#,
    );
    assert_strings(
        &file,
        &[(
            "literal",
            "This is a literal scalar.\nIt preserves newlines.\n\nAnd blank lines too.\n",
        )],
    );
}

#[test]
fn folded_scalar() {
    let file = parse(
        r#"folded: >
  This is a folded scalar.
  These lines will be folded
  into a single line.

  But blank lines create
  paragraph breaks."#,
    );
    assert_strings(&file, &[("folded", "This is a folded scalar. These lines will be folded into a single line.\nBut blank lines create paragraph breaks.\n")]);
}

// Section 2.3: Anchors and aliases

#[test]
fn simple_anchor_and_alias() {
    let file = parse(
        r#"first: &anchor_name
  name: Shared Value
  data: 123
second: *anchor_name"#,
    );
    assert_strings(
        &file,
        &[("first.name", "Shared Value"), ("first.data", "123")],
    );
    // The alias is kept, not dereferenced.
    assert_aliases(&file, &[("second", "anchor_name")]);
}

#[test]
fn merge_keys() {
    let file = parse(
        r#"defaults: &defaults
  adapter: postgres
  host: localhost

development:
  database: dev_db
  <<: *defaults

test:
  database: test_db
  <<: *defaults"#,
    );
    assert_strings(
        &file,
        &[
            ("defaults.adapter", "postgres"),
            ("defaults.host", "localhost"),
            ("development.database", "dev_db"),
            ("test.database", "test_db"),
        ],
    );
    for key in ["development", "test"] {
        let node = node_at(&file, key);
        let merged = merge_value(node.as_mapping().unwrap());
        assert_eq!(merged.as_alias().unwrap().name(), "defaults", "{key}");
    }
}

#[test]
fn multiple_anchors() {
    let file = parse(
        r#"colors:
  - &red '#FF0000'
  - &green '#00FF00'
  - &blue '#0000FF'

theme:
  primary: *blue
  secondary: *green
  danger: *red"#,
    );
    assert_shapes(&file, &[("colors", "block sequence of 3")]);
    assert_strings(
        &file,
        &[
            ("colors.0", "#FF0000"),
            ("colors.1", "#00FF00"),
            ("colors.2", "#0000FF"),
        ],
    );
    assert_aliases(
        &file,
        &[
            ("theme.primary", "blue"),
            ("theme.secondary", "green"),
            ("theme.danger", "red"),
        ],
    );
}

#[test]
fn circular_reference() {
    let file = parse(
        r#"node: &node
  next: *node"#,
    );
    assert_aliases(&file, &[("node.next", "node")]);
}

// Section 3.1: Directives

#[test]
fn directives() {
    for (yaml, expected) in [
        (
            r#"%YAML 1.2
---
key: value"#,
            &[("key", "value")][..],
        ),
        (
            r#"%YAML 1.2
%TAG ! tag:example.com,2014:
%TAG !! tag:example.com,2014:app/
---
data: value"#,
            &[("data", "value")],
        ),
        // Reserved directives are kept but otherwise ignored.
        (
            r#"%RESERVED directive
---
key: value"#,
            &[("key", "value")],
        ),
    ] {
        let file = parse(yaml);
        let expected_keys: Vec<&str> = expected.iter().map(|(k, _)| *k).collect();
        assert_eq!(keys(&file), expected_keys);
        assert_strings(&file, expected);
    }
}

#[test]
fn tag_directive() {
    let file = parse(
        r#"%TAG ! tag:example.com,2014:
---
!foo "bar""#,
    );
    let document = file.document().unwrap();
    let root_node = document.syntax().children().next().unwrap();
    let tagged = TaggedNode::cast(root_node).expect("Root should be a tagged node");
    assert_eq!(tagged.tag(), Some("!foo".to_string()));
    assert_eq!(tagged.value().unwrap().as_string(), "bar");
}

#[test]
fn non_specific_tags() {
    parse(
        r#"plain: !  value
quoted: ! "value""#,
    );
}

// Section 3.2: Document boundaries

#[test]
fn documents() {
    for (yaml, expected) in [
        (
            r#"---
document: one
..."#,
            &[("document", "one")][..],
        ),
        (
            r#"---
first: document
---
second: document
---
third: document"#,
            &[("first", "document")],
        ),
        (
            r#"key: value
another: value"#,
            &[("key", "value"), ("another", "value")],
        ),
    ] {
        assert_strings(&parse(yaml), expected);
    }
}

#[test]
fn empty_documents_in_stream() {
    let file = parse(
        r#"---
...
---
just a string
---
key: value
..."#,
    );
    assert!(file.document().unwrap().is_empty());
}

// Section 5.3: Indicator characters

#[test]
fn reserved_indicators() {
    let file = parse(
        r#"# Comment
key: value  # Inline comment
sequence:
  - item1
  - item2
mapping: {a: 1, b: 2}
anchor: &test value
alias: *test
literal: |
  text
folded: >
  text
tag: !str "value"
directive: value"#,
    );
    assert_strings(
        &file,
        &[
            ("key", "value"),
            ("sequence.0", "item1"),
            ("anchor", "value"),
        ],
    );
    assert_shapes(
        &file,
        &[
            ("sequence", "block sequence of 2"),
            ("mapping", "flow mapping of 2"),
        ],
    );
    assert_aliases(&file, &[("alias", "test")]);
}

// Section 5.7: Escape sequences

#[test]
fn escape_sequences_in_double_quotes() {
    let file = parse(
        r#"escaped: "null: \0, bell: \a, backspace: \b, tab: \t, newline: \n"
more: "vertical: \v, form: \f, return: \r, escape: \e, quote: \", backslash: \\"
unicode: "4-digit: \u263A, 8-digit: \U0001F600"
hex: "hex: \x41""#,
    );
    assert_eq!(keys(&file), ["escaped", "more", "unicode", "hex"]);
}

#[test]
fn unicode_escapes() {
    let file = parse(
        r#"unicode_escape: "\u263A\U0001F600"
emoji: "😀🎉🚀"
special_chars: "Line1\nLine2\tTabbed""#,
    );
    assert_eq!(keys(&file), ["unicode_escape", "emoji", "special_chars"]);
    assert_strings(
        &file,
        &[
            ("unicode_escape", "\u{263A}\u{1F600}"),
            ("emoji", "😀🎉🚀"),
            ("special_chars", "Line1\nLine2\tTabbed"),
        ],
    );
}

#[test]
fn line_continuation() {
    let file = parse(
        r#"continued: "This is a \
  continued line"
folded: "Line 1\
  \  Line 2""#,
    );
    assert_eq!(keys(&file), ["continued", "folded"]);
}

// Section 6: Indentation, line folding and comments

#[test]
fn consistent_indentation() {
    let file = parse(
        r#"root:
  level1:
    level2:
      level3: value
    another2: value
  another1: value"#,
    );
    assert_strings(
        &file,
        &[
            ("root.level1.level2.level3", "value"),
            ("root.level1.another2", "value"),
            ("root.another1", "value"),
        ],
    );
}

#[test]
fn zero_indented_sequence() {
    let file = parse(
        r#"items:
- one
- two
- three"#,
    );
    assert_shapes(&file, &[("items", "block sequence of 3")]);
    assert_strings(
        &file,
        &[("items.0", "one"), ("items.1", "two"), ("items.2", "three")],
    );
}

#[test]
fn line_folding_in_plain_scalars() {
    let file = parse(
        r#"description: This is a very long
  description that spans multiple
  lines but should be treated as
  a single value"#,
    );
    assert_strings(&file, &[("description", "This is a very long description that spans multiple lines but should be treated as a single value")]);
}

#[test]
fn comments() {
    let file = parse(
        r#"# Document comment
key: value  # Inline comment

# Comment before item
item: value

list:
  # Comment in list
  - item1
  - item2  # Item comment"#,
    );
    assert_strings(
        &file,
        &[
            ("key", "value"),
            ("item", "value"),
            ("list.0", "item1"),
            ("list.1", "item2"),
        ],
    );
    assert_shapes(&file, &[("list", "block sequence of 2")]);

    let file = parse(
        r#"# This is a
# multi-line comment
# spanning several lines
data: value

# Another block
# of comments
more: data"#,
    );
    assert_strings(&file, &[("data", "value"), ("more", "data")]);
}

#[test]
fn comments_in_flow_collections() {
    let file = parse(
        r#"flow_sequence: [
    item1,  # Comment 1
    item2,  # Comment 2
    item3   # Comment 3
]
flow_mapping: {
    key1: value1,  # Comment A
    key2: value2,  # Comment B
    key3: value3   # Comment C
}"#,
    );
    assert_shapes(
        &file,
        &[
            ("flow_sequence", "flow sequence of 3"),
            ("flow_mapping", "flow mapping of 3"),
        ],
    );
    let items: Vec<String> = node_at(&file, "flow_sequence")
        .as_sequence()
        .unwrap()
        .values()
        .map(|v| v.as_scalar().unwrap().as_string())
        .collect();
    assert_eq!(items, vec!["item1", "item2", "item3"]);
    assert_strings(
        &file,
        &[
            ("flow_mapping.key1", "value1"),
            ("flow_mapping.key2", "value2"),
            ("flow_mapping.key3", "value3"),
        ],
    );
}

// Section 7: Flow styles

#[test]
fn flow_sequence_variations() {
    let file = parse(
        r#"empty: []
single: [one]
multiple: [one, two, three]
trailing: [one, two,]
multiline: [
  one,
  two,
  three
]"#,
    );
    assert_shapes(
        &file,
        &[
            ("empty", "flow sequence of 0"),
            ("single", "flow sequence of 1"),
            ("multiple", "flow sequence of 3"),
            ("trailing", "flow sequence of 2"),
            ("multiline", "flow sequence of 3"),
        ],
    );
}

#[test]
fn flow_mapping_variations() {
    let file = parse(
        r#"empty: {}
single: {key: value}
multiple: {a: 1, b: 2, c: 3}
trailing: {a: 1, b: 2,}
multiline: {
  key1: value1,
  key2: value2
}"#,
    );
    assert_shapes(
        &file,
        &[
            ("empty", "flow mapping of 0"),
            ("single", "flow mapping of 1"),
            ("multiple", "flow mapping of 3"),
            ("trailing", "flow mapping of 2"),
            ("multiline", "flow mapping of 2"),
        ],
    );
    assert_strings(&file, &[("single.key", "value")]);
}

#[test]
fn trailing_commas_in_flow_collections() {
    let file = parse(
        r#"trailing_seq: [a, b, c,]
trailing_map: {a: 1, b: 2,}"#,
    );
    assert_shapes(
        &file,
        &[
            ("trailing_seq", "flow sequence of 3"),
            ("trailing_map", "flow mapping of 2"),
        ],
    );
}

#[test]
fn nested_flow_collections() {
    let file = parse(
        r#"complex: {
  lists: [a, b, [c, d]],
  maps: {x: 1, y: {z: 2}},
  mixed: [{a: 1}, {b: 2}]
}"#,
    );
    assert_shapes(
        &file,
        &[
            ("complex", "flow mapping of 3"),
            ("complex.lists", "flow sequence of 3"),
            ("complex.lists.2", "flow sequence of 2"),
            ("complex.maps", "flow mapping of 2"),
            ("complex.maps.y", "flow mapping of 1"),
            ("complex.mixed", "flow sequence of 2"),
        ],
    );
}

#[test]
fn block_and_flow_nesting() {
    let file = parse(
        r#"mixed:
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
  ]"#,
    );
    assert_shapes(
        &file,
        &[
            ("mixed.block_seq", "block sequence of 3"),
            ("mixed.flow_in_block", "flow mapping of 3"),
            ("mixed.block_in_flow", "flow sequence of 3"),
        ],
    );
    assert_strings(
        &file,
        &[
            ("mixed.flow_in_block.a", "1"),
            ("mixed.block_in_flow.1.nested", "value"),
        ],
    );
}

#[test]
fn complex_keys() {
    let file = parse(
        r#"? complex
  key
: value
? [sequence, key]
: sequence value
? {mapping: key}
: mapping value"#,
    );
    assert_shapes(&file, &[("", "block mapping of 3")]);
}

// Section 8: Block styles

#[test]
fn block_scalar_chomping() {
    for yaml in [
        r#"strip: |-
  text
keep: |
  text
clip: |+
  text"#,
        r#"strip: >-
  text
keep: >
  text
clip: >+
  text"#,
    ] {
        assert_strings(
            &parse(yaml),
            &[("strip", "text"), ("keep", "text\n"), ("clip", "text\n")],
        );
    }
}

#[test]
fn explicit_indentation_indicators() {
    let file = parse(
        r#"literal: |2
  This has explicit
  indentation indicator
folded: >1
 This too"#,
    );
    assert_strings(
        &file,
        &[
            ("literal", "This has explicit\nindentation indicator\n"),
            ("folded", "This too\n"),
        ],
    );
}

// Section 10: Core schema

#[test]
fn null_representations() {
    let file = parse(
        r#"canonical: null
english: Null
tilde: ~
empty:"#,
    );
    for key in ["canonical", "english", "tilde", "empty"] {
        let node = node_at(&file, key);
        let scalar = node.as_scalar().unwrap();
        assert!(!scalar.is_quoted(), "{key}");
        assert!(scalar.is_null(), "{key}");
    }
}

#[test]
fn boolean_representations() {
    let file = parse(
        r#"canonical_true: true
canonical_false: false
english_true: True
english_false: False
caps_true: TRUE
caps_false: FALSE"#,
    );
    for (key, value) in [
        ("canonical_true", true),
        ("canonical_false", false),
        ("english_true", true),
        ("english_false", false),
        ("caps_true", true),
        ("caps_false", false),
    ] {
        let node = node_at(&file, key);
        let scalar = node.as_scalar().unwrap();
        assert!(!scalar.is_quoted(), "{key}");
        assert_eq!(scalar.as_bool(), Some(value), "{key}");
    }
}

#[test]
fn integer_representations() {
    let file = parse(
        r#"decimal: 123
negative: -456
zero: 0
octal: 0o777
hex: 0xFF
binary: 0b1010"#,
    );
    for (key, value) in [
        ("decimal", Some(123)),
        ("negative", Some(-456)),
        ("zero", Some(0)),
        ("octal", Some(0o777)),
        ("hex", Some(0xFF)),
        // The core schema has no binary form, so `0b1010` is a plain string.
        ("binary", None),
    ] {
        let node = node_at(&file, key);
        let scalar = node.as_scalar().unwrap();
        assert!(!scalar.is_quoted(), "{key}");
        assert_eq!(scalar.as_i64(), value, "{key}");
    }
    assert_strings(&file, &[("binary", "0b1010")]);
}

#[test]
fn float_representations() {
    for (yaml, expected) in [
        (
            r#"simple: 3.15
negative: -2.5
scientific: 6.02e23
negative_exp: 1.0e-10
infinity: .inf
neg_infinity: -.inf
not_a_number: .nan"#,
            &[
                ("simple", 3.15),
                ("negative", -2.5),
                ("scientific", 6.02e23),
                ("negative_exp", 1.0e-10),
                ("infinity", f64::INFINITY),
                ("neg_infinity", f64::NEG_INFINITY),
                ("not_a_number", f64::NAN),
            ][..],
        ),
        (
            r#"infinity: .inf
neg_infinity: -.inf
not_a_number: .nan
negative_zero: -.0"#,
            &[
                ("infinity", f64::INFINITY),
                ("neg_infinity", f64::NEG_INFINITY),
                ("not_a_number", f64::NAN),
                ("negative_zero", -0.0),
            ],
        ),
    ] {
        let file = parse(yaml);
        for (key, value) in expected {
            let node = node_at(&file, key);
            let scalar = node.as_scalar().unwrap();
            assert!(!scalar.is_quoted(), "{key}");
            let actual = scalar.as_f64().unwrap();
            if value.is_nan() {
                assert!(actual.is_nan(), "{key}: {actual}");
            } else {
                assert_eq!(actual, *value, "{key}");
            }
        }
    }
}

#[test]
fn empty_values() {
    let file = parse(
        r#"empty_string: ""
empty_single: ''
null_value:
another_null: null
empty_flow_seq: []
empty_flow_map: {}"#,
    );
    assert_strings(&file, &[("empty_string", ""), ("empty_single", "")]);
    for key in ["null_value", "another_null"] {
        assert!(node_at(&file, key).as_scalar().unwrap().is_null(), "{key}");
    }
    assert_shapes(
        &file,
        &[
            ("empty_flow_seq", "flow sequence of 0"),
            ("empty_flow_map", "flow mapping of 0"),
        ],
    );
}

#[test]
fn special_strings_needing_quotes() {
    let file = parse(
        r#"needs_quotes: "true"
also_needs: "123"
special: "@special"
colon: "key: value"
dash: "- item"
question: "? key""#,
    );
    assert_strings(
        &file,
        &[
            ("needs_quotes", "true"),
            ("also_needs", "123"),
            ("special", "@special"),
            ("colon", "key: value"),
            ("dash", "- item"),
            ("question", "? key"),
        ],
    );
}

/// Number formats are kept as written; invalid ones are plain strings.
#[test]
fn number_formats() {
    for (yaml_str, expected) in [
        (
            r#"
binary1: 0b1010
binary2: 0B1111
negative_binary: -0b1010
positive_binary: +0b101
"#,
            &[
                ("binary1", "0b1010"),
                ("binary2", "0B1111"),
                ("negative_binary", "-0b1010"),
                ("positive_binary", "+0b101"),
            ][..],
        ),
        (
            r#"
octal1: 0o755
octal2: 0O644
negative_octal: -0o755
positive_octal: +0o644
"#,
            &[
                ("octal1", "0o755"),
                ("octal2", "0O644"),
                ("negative_octal", "-0o755"),
                ("positive_octal", "+0o644"),
            ],
        ),
        (
            r#"
empty_binary: 0b
invalid_binary: 0b1012
empty_octal: 0o
invalid_octal: 0o789
invalid_hex: 0xGH
"#,
            &[
                ("empty_binary", "0b"),
                ("invalid_binary", "0b1012"),
                ("empty_octal", "0o"),
                ("invalid_octal", "0o789"),
                ("invalid_hex", "0xGH"),
            ],
        ),
        (
            r#"
zero: 0
double_zero: 00
triple_zero: 000
octal_zero: 0o0
binary_zero: 0b0
hex_zero: 0x0
"#,
            &[
                ("zero", "0"),
                ("double_zero", "00"),
                ("triple_zero", "000"),
                ("octal_zero", "0o0"),
                ("binary_zero", "0b0"),
                ("hex_zero", "0x0"),
            ],
        ),
        (
            r#"
decimal: 42
hex_lower: 0xff
hex_upper: 0XFF
binary: 0b1010
modern_octal: 0o755
legacy_octal: 0755
scientific: 1.5e10
float: 3.14159
"#,
            &[],
        ),
        (
            r#"
numbers:
  - 0b1010
  - 0o755
  - 0xFF
  - 42
  - -0b101
  - +0o777
"#,
            &[],
        ),
        (
            r#"
flow_seq: [0b1010, 0o755, 0xFF, 42]
flow_map: {binary: 0b101, octal: 0o644, hex: 0xAB}
"#,
            &[],
        ),
    ] {
        let doc = parse(yaml_str).document().unwrap();
        for (key, value) in expected {
            assert_eq!(doc.get_string(key), Some(value.to_string()), "{key}");
        }
    }
}

// Chapter 2 examples

#[test]
fn example_2_1_sequence_of_scalars() {
    let file = parse(
        r#"- Mark McGwire
- Sammy Sosa
- Ken Griffey"#,
    );
    assert_shapes(&file, &[("", "block sequence of 3")]);
    assert_strings(
        &file,
        &[
            ("0", "Mark McGwire"),
            ("1", "Sammy Sosa"),
            ("2", "Ken Griffey"),
        ],
    );
}

#[test]
fn example_2_2_mapping_scalars_to_scalars() {
    let file = parse(
        r#"hr:  65    # Home runs
avg: 0.278 # Batting average
rbi: 147   # Runs Batted In"#,
    );
    assert_shapes(&file, &[("", "block mapping of 3")]);
    assert_strings(&file, &[("hr", "65"), ("avg", "0.278"), ("rbi", "147")]);
}

#[test]
fn example_2_3_mapping_scalars_to_sequences() {
    let file = parse(
        r#"american:
  - Boston Red Sox
  - Detroit Tigers
  - New York Yankees
national:
  - New York Mets
  - Chicago Cubs
  - Atlanta Braves"#,
    );
    assert_shapes(
        &file,
        &[
            ("american", "block sequence of 3"),
            ("national", "block sequence of 3"),
        ],
    );
    assert_strings(
        &file,
        &[
            ("american.0", "Boston Red Sox"),
            ("american.1", "Detroit Tigers"),
            ("american.2", "New York Yankees"),
            ("national.0", "New York Mets"),
            ("national.1", "Chicago Cubs"),
            ("national.2", "Atlanta Braves"),
        ],
    );
}

#[test]
fn example_2_4_sequence_of_mappings() {
    let file = parse(
        r#"-
  name: Mark McGwire
  hr:   65
  avg:  0.278
-
  name: Sammy Sosa
  hr:   63
  avg:  0.288"#,
    );
    assert_shapes(&file, &[("", "block sequence of 2")]);
    assert_strings(
        &file,
        &[
            ("0.name", "Mark McGwire"),
            ("0.hr", "65"),
            ("0.avg", "0.278"),
            ("1.name", "Sammy Sosa"),
            ("1.hr", "63"),
            ("1.avg", "0.288"),
        ],
    );
}

#[test]
fn example_2_7_two_documents_in_a_stream() {
    let file = parse(
        r#"# Ranking of 1998 home runs
---
- Mark McGwire
- Sammy Sosa
- Ken Griffey

# Team ranking
---
- Chicago Cubs
- St Louis Cardinals"#,
    );
    assert_shapes(&file, &[("", "block sequence of 3")]);
    assert_strings(
        &file,
        &[
            ("0", "Mark McGwire"),
            ("1", "Sammy Sosa"),
            ("2", "Ken Griffey"),
        ],
    );
}

#[test]
fn example_2_8_play_by_play() {
    let file = parse(
        r#"---
time: 20:03:20
player: Sammy Sosa
action: strike (miss)
...
---
time: 20:03:47
player: Sammy Sosa
action: grand slam
..."#,
    );
    assert_strings(
        &file,
        &[
            ("time", "20:03:20"),
            ("player", "Sammy Sosa"),
            ("action", "strike (miss)"),
        ],
    );
}

#[test]
fn example_2_10_node_for_sammy_sosa() {
    let file = parse(
        r#"---
hr:
  - Mark McGwire
  # Following node labeled SS
  - &SS Sammy Sosa
rbi:
  - *SS # Subsequent occurrence
  - Ken Griffey"#,
    );
    assert_shapes(
        &file,
        &[
            ("hr", "block sequence of 2"),
            ("rbi", "block sequence of 2"),
        ],
    );
    assert_strings(
        &file,
        &[
            ("hr.0", "Mark McGwire"),
            ("hr.1", "Sammy Sosa"),
            ("rbi.1", "Ken Griffey"),
        ],
    );
    assert_aliases(&file, &[("rbi.0", "SS")]);
}

#[test]
fn verbatim_collection_tags_convert() {
    use std::collections::BTreeSet;
    use yaml_edit::YamlValue;
    // `!<tag:yaml.org,2002:set>` is the tag `!!set` abbreviates.
    let value = YamlValue::parse_raw("!<tag:yaml.org,2002:set> {x, y}\n").unwrap();
    assert_eq!(
        value,
        YamlValue::Set(BTreeSet::from(["x".to_string(), "y".to_string()]))
    );
    let value = YamlValue::parse_raw("!<tag:yaml.org,2002:omap> [b: 1]\n").unwrap();
    assert_eq!(
        value,
        YamlValue::OrderedMapping(vec![("b".to_string(), YamlValue::from(1))])
    );
    let value = YamlValue::parse_raw("!<tag:yaml.org,2002:pairs> [b: 1, b: 2]\n").unwrap();
    assert_eq!(
        value,
        YamlValue::Pairs(vec![
            ("b".to_string(), YamlValue::from(1)),
            ("b".to_string(), YamlValue::from(2))
        ])
    );
    let value = YamlValue::parse_raw("!!omap [!<tag:yaml.org,2002:map> {b: 1}]\n").unwrap();
    assert_eq!(
        value,
        YamlValue::OrderedMapping(vec![("b".to_string(), YamlValue::from(1))])
    );
}
