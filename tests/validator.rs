//! What the validator reports about input the parser accepts: which rule
//! each violation breaks, its message and its location.

mod common;
use common::violations;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::validator::{Rule, Severity, Validator, ValidatorConfig, Violation};
use yaml_edit::{Document, YamlFile};

#[test]
fn accepts_well_formed_flow_collections() {
    // The separators in a flow collection live inside the entry they
    // follow, so counting only the collection's direct children found no
    // commas at all and every well-formed flow collection was reported as
    // missing them. Flow entries are also SEQUENCE_ENTRY nodes, which used
    // to read as block syntax leaking into the flow.
    for src in [
        "a: {x: 1, y: 2}\n",
        "a: [1, 2]\n",
        "[1, 2, 3]\n",
        "{a: 1, b: 2, c: 3}\n",
        "a: !!seq [1, 2]\n",
        "a: &anc [1, 2]\n",
    ] {
        let doc = Document::from_str(src).unwrap();
        assert_eq!(Validator::new().validate(&doc), vec![], "{src:?}");
    }
}

#[test]
fn flags_malformed_flow_collections() {
    // Omitted entries and a stray block dash stay errors, and so do the
    // layout mistakes that the comma check used to catch only by
    // accident (yaml-test-suite 9C9N, ZXT5, 9MAG, CTN5, G5U8, YJV2).
    let empty = ("Empty entry in flow collection", Rule::Other);
    let dash = ("Plain scalar cannot start with '-'", Rule::Other);
    for (src, expected) in [
        ("[ , a, b ]\n", vec![empty]),
        ("[ a, b, , ]\n", vec![empty]),
        ("[-]\n", vec![dash]),
        ("- [-, -]\n", vec![dash, dash]),
        (
            "flow: [a,\nb,\nc]\n",
            vec![(
                "Flow collection continuation line must be indented",
                Rule::InvalidIndentation,
            )],
        ),
        (
            "[ \"key\"\n  :value ]\n",
            vec![("Implicit key cannot span multiple lines", Rule::Other)],
        ),
    ] {
        let expected: Vec<_> = expected
            .into_iter()
            .map(|(m, r)| (m.to_string(), r))
            .collect();
        assert_eq!(document_violations(src), expected, "{src:?}");
    }
}

#[test]
fn inconsistent_sequence_indent() {
    assert_eq!(document_violations("- a\n- b\n"), vec![]);

    // `- a\n - b\n` is not two misaligned entries: a `-` indented past
    // the entry above it is plain-scalar content, so this is the single
    // item `a - b`, as both saphyr and PyYAML read it. The parser folds
    // it, leaving one DASH token and nothing for this rule to flag.
    let folded = Document::from_str("- a\n - b\n").unwrap();
    let folded_seq = folded.as_sequence().expect("sequence");
    assert_eq!(folded_seq.len(), 1);
    assert_eq!(
        folded_seq.get(0).unwrap().as_scalar().unwrap().as_string(),
        "a - b"
    );
    assert_eq!(document_violations("- a\n - b\n"), vec![]);
}

#[test]
fn undefined_alias() {
    let undefined = |src: &str| -> Vec<(String, yaml_edit::TextPosition)> {
        let yaml = YamlFile::from_str(src).unwrap();
        Validator::new()
            .validate_syntax(yaml.syntax())
            .into_iter()
            .filter(|v| v.rule == Rule::InvalidAnchor)
            .map(|v| (v.message, v.text_range.unwrap()))
            .collect()
    };
    let at = |start: u32, end: u32| yaml_edit::TextPosition::new(start, end);

    assert_eq!(
        undefined("a: *x\n"),
        vec![("Undefined alias *x".to_string(), at(3, 5))]
    );
    // The anchor must precede the alias.
    assert_eq!(
        undefined("a: *x\nb: &x 1\n"),
        vec![("Undefined alias *x".to_string(), at(3, 5))]
    );
    // Anchors do not carry over into the next document.
    assert_eq!(
        undefined("a: &x 1\n---\nb: *x\n"),
        vec![("Undefined alias *x".to_string(), at(15, 17))]
    );
    assert_eq!(undefined("a: &x 1\nb: *x\n"), vec![]);
    assert_eq!(
        undefined("--- &x 1\n--- &x 2\n--- [*x]\n"),
        vec![("Undefined alias *x".to_string(), at(23, 25))]
    );
    assert_eq!(undefined("[&x a, *x, {*x : b}]\n"), vec![]);
    // `*x` and `&x` in scalar text are content, not node properties.
    assert_eq!(undefined("a: |\n  *x\n"), vec![]);
    assert_eq!(undefined("a: >\n  text\n  *x\n"), vec![]);
    assert_eq!(undefined("a: b\n  *x\n"), vec![]);
    assert_eq!(
        undefined("a: |\n  &x\nb: *x\n"),
        vec![("Undefined alias *x".to_string(), at(13, 15))]
    );
    assert_eq!(
        undefined("a: b\n  &x\nc: *x\n"),
        vec![("Undefined alias *x".to_string(), at(13, 15))]
    );
}

#[test]
fn duplicate_directive() {
    let yaml = "%YAML 1.2\n%YAML 1.2\n---\nkey: value\n";
    let doc = YamlFile::from_str(yaml).unwrap().document().unwrap();
    let validator = Validator::new();
    let violations = validator.validate(&doc);

    assert_eq!(
        violations.len(),
        1,
        "Expected exactly one violation for duplicate YAML directive"
    );
    assert_eq!(violations[0].message, "Duplicate %YAML directive");
}

#[test]
fn duplicate_keys_outside_i64() {
    let doc = Document::from_str(
            "18446744073709551616: a\n0x10000000000000000: b\n9223372036854775808: c\n+9223372036854775808: d\n",
        )
        .unwrap();
    let messages: Vec<_> = Validator::new()
        .validate(&doc)
        .into_iter()
        .filter(|v| v.rule == Rule::DuplicateKeys)
        .map(|v| v.message)
        .collect();
    assert_eq!(
            messages,
            vec![
                "Duplicate key: \"0x10000000000000000\" (semantically equal to \"18446744073709551616\")",
                "Duplicate key: \"+9223372036854775808\" (semantically equal to \"9223372036854775808\")",
            ]
        );
}

#[test]
fn content_after_doc_end() {
    // Test 3HFZ: Content after document end marker. The suite marks it
    // an error case, and the parser now reports it, so take the tree
    // from parse(); the validator still detects it from the ERROR node.
    let yaml = "---\nkey: value\n... invalid\n";
    let parsed = YamlFile::parse(yaml);
    assert_eq!(parsed.errors().len(), 1);
    let doc = parsed.tree().document().unwrap();

    let validator = Validator::new();
    let violations = validator.validate(&doc);

    let invalid_content_violations: Vec<_> = violations
        .iter()
        .filter(|v| v.message.starts_with("Invalid content in document:"))
        .collect();
    assert_eq!(
        invalid_content_violations.len(),
        1,
        "Expected exactly one 'Invalid content' violation for content after document end marker"
    );
}

#[test]
fn directive_with_tagged_node_content() {
    // A document with a tagged scalar following a directive should NOT be
    // reported as "directive without content" - TAGGED_NODE is real content.
    let yaml = "%YAML 1.2\n---\n!custom foo\n";
    let doc = YamlFile::from_str(yaml).unwrap().document().unwrap();
    let validator = Validator::new();
    let violations = validator.validate(&doc);

    assert_eq!(
        violations.len(),
        0,
        "Tagged scalar is real content; valid document should have no violations"
    );
}

#[test]
fn violation_display() {
    let violation = Violation {
        message: "Test violation".to_string(),
        location: Some("1:5".to_string()),
        text_range: None,
        severity: Severity::Error,
        rule: Rule::InvalidIndentation,
    };

    assert_eq!(
        format!("{}", violation),
        "[ERROR] 1:5: Test violation (InvalidIndentation)"
    );
}

#[test]
fn u99r_invalid_comma_in_tag() {
    let file = YamlFile::from_str("- !!str, xxx\n").unwrap();
    let violations = Validator::new().validate_syntax(file.syntax());
    assert_eq!(
        violations
            .iter()
            .map(|v| (v.message.as_str(), v.rule, v.text_range.map(|r| r.start)))
            .collect::<Vec<_>>(),
        vec![("Plain scalar cannot start with ','", Rule::Other, Some(7))]
    );
}

#[test]
fn explicit_value_at_its_key_column() {
    let messages = |yaml: &str| -> Vec<(String, Option<u32>)> {
        let file = YamlFile::from_str(yaml).unwrap();
        Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.text_range.map(|r| r.start)))
            .collect()
    };
    let error = "Explicit value indicator ':' must be at the column of its '?'";
    for (yaml, start) in [
        ("? a\n : b\n", 5),
        ("c:\n ?  - a\n  : b\n", 13),
        ("c:\n ?  []\n  : b\n", 12),
    ] {
        assert_eq!(
            messages(yaml),
            vec![(error.to_string(), Some(start))],
            "{yaml:?}"
        );
    }
    for yaml in [
        "? a\n: b\n",
        "- ? a\n  : b\n",
        "? - a\n  - b\n: c\n",
        "{ ? a\n : b }\n",
    ] {
        assert_eq!(messages(yaml), vec![], "{yaml:?}");
    }
}

#[test]
fn flow_indicator_in_a_flow_plain_scalar() {
    let messages = |yaml: &str| -> Vec<(String, Option<u32>)> {
        let file = YamlFile::from_str(yaml).unwrap();
        Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.text_range.map(|r| r.start)))
            .collect()
    };
    for (yaml, error, start) in [
        (
            "[a{b]\n",
            "Plain scalar in a flow collection cannot contain \"{\"",
            2,
        ),
        (
            "{a[b}\n",
            "Plain scalar in a flow collection cannot contain \"[\"",
            2,
        ),
        (
            "- {ye\"k \"{ye\"k \":vaa_e }\n",
            "Plain scalar in a flow collection cannot contain \"{\"",
            9,
        ),
    ] {
        assert_eq!(
            messages(yaml),
            vec![(error.to_string(), Some(start))],
            "{yaml:?}"
        );
    }
    for yaml in ["a{b: [c]\n", "[a, {b: c}]\n", "[\"a{b\"]\n"] {
        assert_eq!(messages(yaml), vec![], "{yaml:?}");
    }
}

#[test]
fn plain_scalar_cannot_start_with_an_indicator() {
    let messages = |yaml: &str| -> Vec<(String, Option<u32>)> {
        let file = YamlFile::from_str(yaml).unwrap();
        Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.text_range.map(|r| r.start)))
            .collect()
    };
    for (yaml, error, start) in [
        ("]}]}", "Plain scalar cannot start with ']'", 0),
        ("a: ,b\n", "Plain scalar cannot start with ','", 3),
        ("- a\n- }\n", "Plain scalar cannot start with '}'", 6),
        ("[-1, - 2]", "Plain scalar cannot start with '-'", 5),
        ("[a, -]", "Plain scalar cannot start with '-'", 4),
        ("{- : 1}", "Plain scalar cannot start with '-'", 1),
        ("a: @x\n", "Plain scalar cannot start with '@'", 3),
        ("- `x\n", "Plain scalar cannot start with '`'", 2),
        ("[%x]", "Plain scalar cannot start with '%'", 1),
        ("!x - a\n", "Plain scalar cannot start with '-'", 3),
        ("k: !x - a\n", "Plain scalar cannot start with '-'", 6),
        ("- &a - a\n", "Plain scalar cannot start with '-'", 5),
        ("- !x -\n", "Plain scalar cannot start with '-'", 5),
        (
            "&anchor - sequence entry\n",
            "Plain scalar cannot start with '-'",
            8,
        ),
    ] {
        assert_eq!(
            messages(yaml),
            vec![(error.to_string(), Some(start))],
            "{yaml:?}"
        );
    }
    for yaml in [
        "[-1, -a]",
        "{-: 1}",
        "a: x]\n",
        "a: x,y\n",
        "[a\n -]",
        "a: x%y@z`\n",
        "a: '@x'\n",
        "a: b\n  - c\n",
        "k: !x -1\n",
        "- &a -a\n",
        "!x -[a]\n",
    ] {
        assert_eq!(messages(yaml), vec![], "{yaml:?}");
    }
}

#[test]
fn document_marker_in_quoted_scalar() {
    let messages = |yaml: &str| -> Vec<String> {
        let file = YamlFile::from_str(yaml).unwrap();
        Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| v.message)
            .collect()
    };
    for yaml in [
        "a: \"x\n---\ny\"\n",
        "'x\n... y'\n",
        "--- \"a\n...\tb\"\n",
        "--- \"a\n--- b\"\n",
        "---\n\"\n---\n\"\n",
    ] {
        assert_eq!(
            messages(yaml),
            vec!["Document marker inside a quoted scalar".to_string()],
            "{yaml:?}"
        );
    }
    for yaml in [
        "\"x\n---y\"\n",
        "--- \"a\n---\"\n",
        "'a\n...x'\n",
        "--- \"a\n---x\nb\"\n",
        "--- 'a\n...'\n",
    ] {
        assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
    }
}

#[test]
fn comment_after_empty_value() {
    // The empty value of `? |` sits between the line break and the
    // comment, and separates nothing.
    let parsed = YamlFile::from_str("? |\n# c\n").unwrap();
    assert_eq!(Validator::new().validate_syntax(parsed.syntax()), vec![]);
}

#[test]
fn anchor_or_alias_without_name() {
    let validator = Validator::new();
    for (yaml, message, start) in [
        ("&\n", "Anchor without a name", 0),
        ("a: &\n", "Anchor without a name", 3),
        ("{&}\n", "Anchor without a name", 1),
        ("- *\n", "Alias without a name", 2),
        ("{*}\n", "Alias without a name", 1),
    ] {
        let parsed = YamlFile::from_str(yaml).unwrap();
        let found: Vec<_> = validator
            .validate_syntax(parsed.syntax())
            .into_iter()
            .map(|v| (v.message, v.text_range.map(|r| r.start)))
            .collect();
        assert_eq!(found, [(message.to_string(), Some(start))], "{yaml:?}");
    }
}

#[test]
fn doc_start_block_sequence() {
    assert_eq!(
        located("--- - 1\n"),
        vec![(
            "Content on same line as document start marker".to_string(),
            "1:1".to_string()
        )]
    );
    assert_eq!(
        located("--- - 1\n- 2\n"),
        vec![
            (
                "Content on same line as document start marker".to_string(),
                "1:1".to_string()
            ),
            (
                "Inconsistent sequence item indentation".to_string(),
                "2:1".to_string()
            )
        ]
    );
    assert_eq!(located("--- # c\n- 1\n"), vec![]);
}

fn messages(yaml: &str) -> Vec<String> {
    let file = YamlFile::from_str(yaml).unwrap();
    Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .map(|v| v.message)
        .collect()
}

/// What the validator reports about `yaml` as (message, location) pairs.
fn located(yaml: &str) -> Vec<(String, String)> {
    violations(&YamlFile::from_str(yaml).unwrap())
}

#[test]
fn location_line_breaks_and_bom() {
    for (yaml, location) in [
        ("a: 1\nb: {x: 1, x: 2}\n", "2:11"),
        ("a: 1\rb: {x: 1, x: 2}\r", "2:11"),
        ("a: 1\r\nb: {x: 1, x: 2}\r\n", "2:11"),
        ("\u{FEFF}b: {x: 1, x: 2}\n", "1:11"),
    ] {
        assert_eq!(
            located(yaml),
            vec![(
                "Duplicate key: \"x\" (semantically equal to \"x\")".to_string(),
                location.to_string()
            )],
            "{yaml:?}"
        );
    }
}

#[test]
fn indentation_with_cr_line_breaks() {
    assert_eq!(located("-\r  name: a\r  hr: 1\r-\r  name: b\r"), vec![]);
    assert_eq!(located("a:\r  b: 1\r  c: 2\r"), vec![]);
}

#[test]
fn quoted_scalar_lines_with_cr_line_breaks() {
    let marker = "Document marker inside a quoted scalar";
    let wrong_indent = "Wrong indented multiline quoted scalar";
    for (yaml, expected) in [
        ("---\r\"\r---\r\"\r", vec![(marker, "2:1")]),
        ("---\r'\r...\r'\r", vec![(marker, "2:1")]),
        (
            "- - \"bar\rbar\": x\r",
            vec![
                ("Implicit key cannot span multiple lines", "1:5"),
                (wrong_indent, "1:5"),
            ],
        ),
        ("---\rquoted: \"a\rb\rc\"\r", vec![(wrong_indent, "2:9")]),
        (
            "foo: \"bar\r\tbaz\"\r",
            vec![("Tabs are not allowed for indentation in YAML", "1:6")],
        ),
    ] {
        let expected: Vec<_> = expected
            .into_iter()
            .map(|(m, l)| (m.to_string(), l.to_string()))
            .collect();
        assert_eq!(located(yaml), expected, "{yaml:?}");
    }
}

#[test]
fn block_scalar_in_flow_collection() {
    for (yaml, location) in [
        ("[|\n  x\n]\n", "1:2"),
        ("[k: |\n  x\n]\n", "1:5"),
        ("{k: >\n  x\n}\n", "1:5"),
        ("- [a, !!str >-\n    x\n  ]\n", "1:13"),
        ("{|: x}\n", "1:2"),
    ] {
        assert_eq!(
            located(yaml),
            vec![(
                "Block scalar cannot appear inside a flow collection".to_string(),
                location.to_string()
            )],
            "{yaml:?}"
        );
    }
    for yaml in ["[a|b, c>d]\n", "k: |\n  [x]\n", "{k: '|'}\n", "[\"> x\"]\n"] {
        assert_eq!(located(yaml), vec![], "{yaml:?}");
    }
}

#[test]
fn flow_entries_need_commas() {
    // A trailing comma used to make up for one missing between entries.
    for (yaml, locations) in [
        ("[k:\n  x: 1,\n]\n", vec!["2:4"]),
        ("{a: 1 b: 2, }\n", vec!["1:8"]),
        ("{foo: 1 bar: 2}\n", vec!["1:12"]),
        ("{foo: 1 bar: 2}", vec!["1:12"]),
        ("[a: b: c, d: e: f]\n", vec!["1:6", "1:15"]),
        ("{\n foo: 1\n bar: 2 }\n", vec!["3:5"]),
    ] {
        assert_eq!(
            located(yaml),
            locations
                .into_iter()
                .map(|l| (
                    "Missing comma between flow collection entries".to_string(),
                    l.to_string()
                ))
                .collect::<Vec<_>>(),
            "{yaml:?}"
        );
    }
    for yaml in ["[a, b c, ]\n", "{a: 1, b: 2,}\n", "[a,\n b]\n", "[a b]\n"] {
        assert_eq!(located(yaml), vec![], "{yaml:?}");
    }
}

#[test]
fn flow_pair_only_as_sequence_entry() {
    for (yaml, location) in [
        ("[k: ? a]\n", "1:5"),
        ("{k: ? a}\n", "1:5"),
        ("[k: ? a: b]\n", "1:5"),
        ("- {x: [y, k: ? a]}\n", "1:14"),
    ] {
        assert_eq!(
            located(yaml),
            vec![(
                "Mapping without braces can only be a flow sequence entry".to_string(),
                location.to_string()
            )],
            "{yaml:?}"
        );
    }
    for yaml in [
        "[? a]\n",
        "[k: v]\n",
        "[[? a: b]]\n",
        "{? a}\n",
        "[? a\n? b]\n",
    ] {
        assert_eq!(located(yaml), vec![], "{yaml:?}");
    }
}

#[test]
fn key_holds_one_node() {
    for (yaml, location) in [
        ("? a\n 5b: 1\n", "2:2"),
        ("? a\n  5b: 1\n", "2:3"),
        ("? !t a\n 5b: 1\n", "2:2"),
        ("? [a]\n 5b: 1\n", "2:2"),
        ("- ? a\n   5b: 1\n", "2:4"),
    ] {
        assert_eq!(
            located(yaml),
            vec![(
                "Key holds more than one node".to_string(),
                location.to_string()
            )],
            "{yaml:?}"
        );
    }
    for yaml in [
        "? a5\n",
        "? a\n5b: 1\n",
        "? a\n:5b: 1\n",
        "? a\n  b\n",
        "? &x\n  5b: 1\n",
        "? !t\n  5b: 1\n",
        "x:\n  ? a\n  5b: 1\n",
        "- ? a\n  5b: 1\n",
    ] {
        assert_eq!(located(yaml), vec![], "{yaml:?}");
    }
}

/// A block mapping whose first key is a flow collection starts with
/// that collection's `{` or `[`, but is still a block mapping.
#[test]
fn block_mapping_with_flow_collection_key_locations() {
    for yaml in [
        "{a}: x\nb: c\n",
        "{a: 1}: x\n[b]: c\n",
        "k:\n  {a}: x\n  b: y\n",
    ] {
        assert_eq!(located(yaml), vec![], "{yaml:?}");
    }
    for (yaml, message, location) in [
        (
            "{a}: x\n b: c\n",
            "Sibling block mapping entries have inconsistent indentation \
                 (expected \"\", found \" \")",
            "2:1",
        ),
        (
            "--- {a}: x\n",
            "Content on same line as document start marker",
            "1:1",
        ),
        (
            "--- [a]: x\n",
            "Content on same line as document start marker",
            "1:1",
        ),
        (
            "k:\n \t{a}: x\n",
            "Tabs are not allowed for indentation in YAML",
            "2:1",
        ),
        (
            "{a}: x\ny: [b,\nc]\n",
            "Flow collection continuation line must be indented",
            "2:4",
        ),
    ] {
        assert_eq!(
            located(yaml),
            vec![(message.to_string(), location.to_string())],
            "{yaml:?}"
        );
    }
}

#[test]
fn compact_mapping_in_explicit_entry() {
    for yaml in [
        "? a: 1\n  b: 2\n",
        "?   a: 1\n    b: 2\n",
        "? x\n: a: 1\n  b: 2\n",
        "- ? a: 1\n    b: 2\n  : c: 3\n    d: 4\n",
    ] {
        assert_eq!(located(yaml), vec![], "{yaml:?}");
    }
    assert_eq!(
        located("? a: 1\n b: 2\n"),
        vec![(
            "Sibling block mapping entries have inconsistent indentation \
                 (expected \"  \", found \" \")"
                .to_string(),
            "2:1".to_string()
        )]
    );
}

#[test]
fn indented_root_mapping() {
    for yaml in [
        "  a: 1\n  b: 2\n",
        "\n  a: 1\n  b: 2\n",
        "# c\n  a: 1\n  b: 2\n",
        "---\n  a: 1\n  b: 2\n",
        "--- # c\n  a: 1\n  b: 2\n",
        "a: 1\n---\n  b: 1\n  c: 2\n",
        "  - a\n  - b\n",
        "# c\n  - a\n  - b\n",
    ] {
        assert_eq!(located(yaml), vec![], "{yaml:?}");
    }
    // `s-indent` is spaces only, so a tab cannot indent the first key.
    assert_eq!(
        located("\ta: 1\n b: 2\n"),
        vec![(
            "Tabs are not allowed for indentation in YAML".to_string(),
            "1:1".to_string()
        )]
    );
    assert_eq!(
        located("  a: 1\n   b: 2\n"),
        vec![(
            "Sibling block mapping entries have inconsistent indentation \
                 (expected \"  \", found \"   \")"
                .to_string(),
            "2:1".to_string()
        )]
    );
}

#[test]
fn directives_apply_per_document() {
    for yaml in [
        "%YAML 1.2\n--- a\n...\n%YAML 1.2\n--- b\n",
        "%TAG !a! tag:a,\n%TAG !b! tag:b,\n--- !a!x 1\n",
        "--- a\n...\n%YAML 1.2\n--- b\n",
        "%TAG !e! tag:a,\n--- !e!x a\n...\n%TAG !e! tag:b,\n--- !e!y b\n",
        "%YAML 1.2\n---\n",
        "%YAML 1.1  # comment\n---\n",
        "%YAM 1.1\n---\n",
        "--- |\n%x\n...\n--- b\n",
        "--- |\n%x\n--- b\n",
        "---\nscalar\n%YAML 1.2\n...\n--- b\n",
    ] {
        assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
    }
}

#[test]
fn validate_only_checks_the_documents_own_directives() {
    let file = YamlFile::from_str("%YAML 1.2\n--- a\n...\n---\n").unwrap();
    for doc in file.documents() {
        assert_eq!(Validator::new().validate(&doc), vec![]);
    }
}

#[test]
fn directive_errors() {
    for (yaml, expected) in [
        (
            "%YAML 1.2\n%YAML 1.2\n---\na\n",
            "Duplicate %YAML directive",
        ),
        (
            "%TAG !a! x\n%TAG !a! y\n---\na\n",
            "Duplicate %TAG directive for !a!",
        ),
        (
            "%YAML 1.2\n...\n",
            "Directive without document start marker (---)",
        ),
        ("%YAML 1.2 foo\n---\n", "Malformed %YAML directive"),
        ("%YAML 1.1#c\n---\n", "Malformed %YAML directive"),
        (
            "a: b\n%YAML 1.2\n--- c\n",
            "Directive after document requires document end marker (...)",
        ),
        (
            "---\n%x\n...\n--- b\n",
            "Directive in document content (missing document end marker `...` before directive)",
        ),
        (
            "%TAG !e! tag:a,\n--- !e!x a\n...\n--- !e!y b\n",
            "Tag shorthand !e! is only defined in the document that declares it",
        ),
    ] {
        assert_eq!(messages(yaml), vec![expected.to_string()], "{yaml:?}");
    }
}

#[test]
fn tabs_that_are_not_indentation() {
    for yaml in [
        "foo: 1\n\t\nbar: 2\n",
        "k: v\n \t\nj: w\n",
        "- a\n- b\n\t\n- c\n",
        "a: [\n  b,\n\t\n  c]\n",
        "a: 1 # c\n# d\n\t# e\nb: 2\n",
        "foo:\n \tbar\n",
        "k: [a,\n  \tb]\n",
        "a:\n \t[x]\n",
        "a:\n \t{x: y}\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn tab_indentation_rejected() {
    // DK95/06 is the third: the tab moves the entry's column.
    for yaml in [
        "a:\n\tb: 1\n",
        "- [\n\tfoo,\n foo\n ]\n",
        "foo:\n  a: 1\n  \tb: 2\n",
        "a:\n \t- x\n",
        "a:\n \tb: 1\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let tab_errors: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .filter(|v| v.rule == Rule::InvalidTabUsage)
            .map(|v| v.message)
            .collect();
        assert_eq!(
            tab_errors,
            vec!["Tabs are not allowed for indentation in YAML".to_string()],
            "{yaml:?}"
        );
    }
}

#[test]
fn error_preview_truncates_on_char_boundary() {
    // The preview used to be cut at byte 50, which panicked when that
    // fell inside a multi-byte character.
    let yaml = format!("a: b\n{}\u{e9}yz\n", "x".repeat(49));
    let (file, _) = YamlFile::from_str_relaxed(&yaml);
    let violations = Validator::new().validate_syntax(file.syntax());
    let messages: Vec<_> = violations.iter().map(|v| v.message.as_str()).collect();
    assert_eq!(
        messages,
        vec![format!(
            "Invalid content in document: \"{}\u{e9}...\"",
            "x".repeat(49)
        )]
    );
}

#[test]
fn columns_ignore_byte_order_mark() {
    for yaml in ["\u{feff}- a\n- b\n", "\u{feff}- a: 1\n  b: 2\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn directive_in_document_content() {
    // %YAML directive after --- without preceding ... is invalid
    let input = "%YAML 1.2\n---\n%YAML 1.2\n---\n";
    let file = YamlFile::from_str(input).unwrap();
    let validator = Validator::new();
    let violations = validator.validate_syntax(file.syntax());

    assert_eq!(
        violations.len(),
        1,
        "Expected one violation for directive in content, got: {:?}",
        violations
    );
    assert_eq!(
        violations[0].message,
        "Directive in document content (missing document end marker `...` before directive)"
    );
}

#[test]
fn nested_mapping_indentation_accepted() {
    for yaml in [
        "k: !t\n  a: 1\n  b: 2\n",
        "- !!map\n  a: 1\n  b: 2\n",
        "- &a !t\n  a: 1\n  b: 2\n",
        "k:\n  ? a\n  ? b\n",
        "set: !!set\n  ? a\n  ? b\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn set_content_must_be_mapping() {
    for (yaml, location) in [
        ("s: !!set [a, b]\n", "1:4"),
        ("- !!set\n  - a\n", "1:3"),
        ("s: !!set\nt: 1\n", "1:4"),
        ("!!set\n", "1:1"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        assert_eq!(
            violations,
            vec![(
                "!!set content is not a mapping".to_string(),
                Some(location.to_string()),
                Severity::Warning,
                Rule::InvalidTag
            )],
            "{yaml:?}"
        );
    }
    for yaml in ["s: !!set {? a}\n", "s: !!set &x\n  ? a\n", "s: !!set {}\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn block_mapping_after_properties() {
    for (yaml, location) in [
        ("!x ? a\n", "1:4"),
        ("&a ? a\n", "1:4"),
        ("k: !x &a ? a\n", "1:10"),
        ("- !x ? a\n", "1:6"),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        assert_eq!(
            violations,
            vec![(
                "A block mapping cannot start on the line of its tag or anchor".to_string(),
                Some(location.to_string()),
                Severity::Error,
                Rule::Other
            )],
            "{yaml:?}"
        );
    }
    for yaml in [
        "!x\n? a\n",
        "&a\n? a\n",
        "!x a: 1\n",
        "k: !x {? a}\n",
        "- ? a\n",
        "? &a a\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn anchor_on_alias() {
    let both = "Node cannot have both an anchor and be an alias";
    for (yaml, expected) in [
        ("a: &a 1\nb:\n- &b *a\n", vec![(both, "3:3", Rule::Other)]),
        ("- &a 1\n- &b *a\n", vec![(both, "2:3", Rule::Other)]),
        (
            "--- &b *a\n",
            vec![
                (both, "1:5", Rule::Other),
                ("Undefined alias *a", "1:8", Rule::InvalidAnchor),
            ],
        ),
        ("a: &a 1\nb: [&b *a]\n", vec![(both, "2:5", Rule::Other)]),
        (
            "a: &a 1\nb: [&b *a : v]\n",
            vec![(both, "2:5", Rule::Other)],
        ),
        (
            "a: &a 1\nb: {&b *a : v}\n",
            vec![(both, "2:5", Rule::Other)],
        ),
        ("a: &a 1\n? &b *a\n: v\n", vec![(both, "2:3", Rule::Other)]),
        ("a: &a 1\n&b *a : v\n", vec![(both, "2:1", Rule::Other)]),
        (
            "a: &a 1\nb:\n- &b *a : v\n",
            vec![(both, "3:3", Rule::Other)],
        ),
        (
            "a: &a 1\nk:\n  &b *a : v\n",
            vec![(both, "3:3", Rule::Other)],
        ),
        ("a: &a 1\n? x\n: &b *a\n", vec![(both, "3:3", Rule::Other)]),
        ("a: &a 1\nk: &b\n  *a\n", vec![(both, "2:4", Rule::Other)]),
        (
            "a: &a 1\nk: &b !t *a\n",
            vec![
                (both, "2:4", Rule::Other),
                ("An alias cannot have a tag", "2:7", Rule::InvalidTag),
            ],
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        let expected: Vec<_> = expected
            .into_iter()
            .map(|(message, location, rule)| {
                (
                    message.to_string(),
                    Some(location.to_string()),
                    Severity::Error,
                    rule,
                )
            })
            .collect();
        assert_eq!(violations, expected, "{yaml:?}");
    }
    for yaml in [
        "a: &a 1\nk: &c !t\n  x: *a\n",
        "a: &a 1\nk: &c !t [*a]\n",
        "a: &a 1\nk: &c [*a]\n",
        "a: &a 1\nk: &c x *a\n",
        "a: &a 1\nt: &n\n  *a : v\n",
        "a: &a 1\nb: &n\n- *a\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn tag_on_alias() {
    for (yaml, expected) in [
        (
            "a: &a 1\nk: !t *a\n",
            vec![("An alias cannot have a tag", "2:4", Rule::InvalidTag)],
        ),
        (
            "- &a 1\n- !t *a\n",
            vec![("An alias cannot have a tag", "2:3", Rule::InvalidTag)],
        ),
        (
            "[&a 1, !t *a]\n",
            vec![("An alias cannot have a tag", "1:8", Rule::InvalidTag)],
        ),
        (
            "a: &a 1\nb: {!!str *a : v}\n",
            vec![("An alias cannot have a tag", "2:5", Rule::InvalidTag)],
        ),
        (
            "a: &a 1\n!t *a : v\n",
            vec![("An alias cannot have a tag", "2:1", Rule::InvalidTag)],
        ),
        (
            "a: &a 1\nb:\n- !t *a : v\n",
            vec![("An alias cannot have a tag", "3:3", Rule::InvalidTag)],
        ),
        (
            "a: &a 1\nb: {!t *a : v}\n",
            vec![("An alias cannot have a tag", "2:5", Rule::InvalidTag)],
        ),
        (
            "a: &a 1\nk: !t\n  *a\n",
            vec![("An alias cannot have a tag", "2:4", Rule::InvalidTag)],
        ),
        (
            "a: &a 1\nk: !t &b *a\n",
            vec![
                (
                    "Node cannot have both an anchor and be an alias",
                    "2:7",
                    Rule::Other,
                ),
                ("An alias cannot have a tag", "2:4", Rule::InvalidTag),
            ],
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        let expected: Vec<_> = expected
            .into_iter()
            .map(|(message, location, rule)| {
                (
                    message.to_string(),
                    Some(location.to_string()),
                    Severity::Error,
                    rule,
                )
            })
            .collect();
        assert_eq!(violations, expected, "{yaml:?}");
    }
    for yaml in ["a: &a 1\nk: !t &b x\nl: *a\n", "a: &a 1\nk: !t\n  *a : v\n"] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn pairs_content_must_be_sequence() {
    for (yaml, location, message) in [
        (
            "o: !!omap {a: 1}\n",
            "1:4",
            "!!omap content is not a sequence",
        ),
        (
            "- !!pairs\n  a: 1\n",
            "1:3",
            "!!pairs content is not a sequence",
        ),
        (
            "o: !!omap\n  3\n",
            "1:4",
            "!!omap content is not a sequence",
        ),
        (
            "- !!omap\n- a: 1\n",
            "1:3",
            "!!omap content is not a sequence",
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        assert_eq!(
            violations,
            vec![(
                message.to_string(),
                Some(location.to_string()),
                Severity::Warning,
                Rule::InvalidTag
            )],
            "{yaml:?}"
        );
    }
    for yaml in [
        "o: !!omap [a: 1]\n",
        "o: !!pairs &x\n  - a: 1\n  - a: 2\n",
        "o: !!omap []\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn tagged_alias_reported_once() {
    for yaml in [
        "a: &a [x]\nb: !!omap *a\n",
        "a: &a [x]\nb: !!pairs *a\n",
        "a: &a {x: }\nb: !!set *a\n",
        "a: &a [x]\nb: !!set *a\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        assert_eq!(
            violations,
            vec![(
                "An alias cannot have a tag".to_string(),
                Some("2:4".to_string()),
                Severity::Error,
                Rule::InvalidTag
            )],
            "{yaml:?}"
        );
    }
}

#[test]
fn pairs_items_must_be_single_entry_mappings() {
    for (yaml, message, location) in [
        (
            "o: !!omap [a]\n",
            "!!omap item \"a\" is not a mapping with a single entry",
            "1:12",
        ),
        (
            "o: !!omap [{a: 1, b: 2}]\n",
            "!!omap item \"{a: 1, b: 2}\" is not a mapping with a single entry",
            "1:12",
        ),
        (
            "o: !!pairs\n- !x {a: 1}\n",
            "!!pairs item \"!x {a: 1}\" is not a mapping with a single entry",
            "2:3",
        ),
        (
            "o: !!pairs [!!map [a]]\n",
            "!!pairs item \"!!map [a]\" is not a mapping with a single entry",
            "1:13",
        ),
        (
            "o: !!omap\n- [a: 1]\n",
            "!!omap item \"[a: 1]\" is not a mapping with a single entry",
            "2:3",
        ),
        (
            "o: !!omap\n- {}\n",
            "!!omap item \"{}\" is not a mapping with a single entry",
            "2:3",
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        assert_eq!(
            violations,
            vec![(
                message.to_string(),
                Some(location.to_string()),
                Severity::Warning,
                Rule::InvalidTag
            )],
            "{yaml:?}"
        );
    }
    for yaml in [
        "o: !!omap [a: 1, b: 2]\n",
        "o: !!pairs [!!map {a: 1}, a: 2]\n",
        "o: !!omap\n- a: 1\n- b:\n",
        "m: &m {a: 1}\no: !!omap [*m]\n",
        "o: [a, {b: 1, c: 2}]\n",
        "o: !x [a]\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn repeated_omap_key() {
    for (yaml, message, location) in [
        (
            "o: !!omap [a: 1, a: 2]\n",
            "Duplicate !!omap key: \"a\" (semantically equal to \"a\")",
            "1:18",
        ),
        (
            "o: !!omap\n- 16: a\n- b: 2\n- 0x10: c\n",
            "Duplicate !!omap key: \"0x10\" (semantically equal to \"16\")",
            "4:3",
        ),
        (
            "o: !!omap [!!map {a: 1}, a: 2]\n",
            "Duplicate !!omap key: \"a\" (semantically equal to \"a\")",
            "1:26",
        ),
        (
            "o: !!omap\n- : 1\n- : 2\n",
            "Duplicate !!omap key: null (semantically equal to null)",
            "3:3",
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        assert_eq!(
            violations,
            vec![(
                message.to_string(),
                Some(location.to_string()),
                Severity::Warning,
                Rule::DuplicateKeys
            )],
            "{yaml:?}"
        );
    }
    for yaml in [
        "o: !!omap [a: 1, b: 2]\n",
        "o: !!omap [1: a, '1': b]\n",
        "o: !!pairs [a: 1, a: 2]\n",
        "o: [a: 1, a: 2]\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
    // An item that is not a mapping has no key to repeat.
    let file = YamlFile::from_str("o: !!omap [!x {a: 1}, a: 2]\n").unwrap();
    let rules: Vec<_> = Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .map(|v| v.rule)
        .collect();
    assert_eq!(rules, vec![Rule::InvalidTag]);
    // A repeated !!set member is a repeated mapping key.
    let file = YamlFile::from_str("s: !!set {? a, ? a}\n").unwrap();
    let violations: Vec<_> = Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .map(|v| (v.message, v.location, v.severity, v.rule))
        .collect();
    assert_eq!(
        violations,
        vec![(
            "Duplicate key: \"? a\" (semantically equal to \"? a\")".to_string(),
            Some("1:16".to_string()),
            Severity::Error,
            Rule::DuplicateKeys
        )]
    );
}

#[test]
fn pairs_items_resolve_merge_keys() {
    let violations = |yaml: &str| -> Vec<_> {
        let file = YamlFile::from_str(yaml).unwrap();
        Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.rule))
            .collect()
    };
    for (yaml, message, location) in [
        (
            "o: !!omap [{<<: {a: 1, b: 2}}]\n",
            "!!omap item \"{<<: {a: 1, b: 2}}\" is not a mapping with a single entry",
            "1:12",
        ),
        (
            "m: &m {a: 1, b: 2}\no: !!pairs\n- <<: *m\n",
            "!!pairs item \"<<: *m\\n\" is not a mapping with a single entry",
            "3:3",
        ),
        (
            "m: &m {a: 1}\no: !!omap [{c: 1, <<: *m}]\n",
            "!!omap item \"{c: 1, <<: *m}\" is not a mapping with a single entry",
            "2:12",
        ),
    ] {
        assert_eq!(
            violations(yaml),
            vec![(
                message.to_string(),
                Some(location.to_string()),
                Rule::InvalidTag
            )],
            "{yaml:?}"
        );
    }
    for (yaml, message, location) in [
        (
            "m: &m {a: 1}\no: !!omap [{<<: *m}, a: 2]\n",
            "Duplicate !!omap key: \"a\" (semantically equal to \"a\")",
            "2:22",
        ),
        (
            "o: !!omap [a: 1, {<<: {a: 2}}]\n",
            "Duplicate !!omap key: \"a\" (semantically equal to \"a\")",
            "1:18",
        ),
    ] {
        assert_eq!(
            violations(yaml),
            vec![(
                message.to_string(),
                Some(location.to_string()),
                Rule::DuplicateKeys
            )],
            "{yaml:?}"
        );
    }
    for yaml in [
        "o: !!omap [{a: 1, <<: {a: 2}}]\n",
        "m: &m {a: 1}\no: !!omap [{a: 2, <<: *m}]\n",
        "m: &m {a: 1}\no: !!pairs [{<<: *m}, a: 2]\n",
        "o: !!omap [{<<: {a: 1}}, {<<: {b: 1}}]\n",
        "o: !!omap [{<<: [{a: 1}, {a: 2}]}]\n",
    ] {
        assert_eq!(violations(yaml), vec![], "{yaml:?}");
    }
}

#[test]
fn set_values_must_be_null() {
    for (yaml, message, location) in [
        (
            "s: !!set {a: 1}\n",
            "!!set member \"a\" has non-null value \"1\"",
            "1:14",
        ),
        (
            "s: !!set\n  a: [x]\n  b:\n",
            "!!set member \"a\" has non-null value \"[x]\"",
            "2:6",
        ),
        (
            "s: !!set\n  ? a\n  : x\n",
            "!!set member \"a\" has non-null value \"x\"",
            "3:5",
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location, v.severity, v.rule))
            .collect();
        assert_eq!(
            violations,
            vec![(
                message.to_string(),
                Some(location.to_string()),
                Severity::Warning,
                Rule::InvalidTag
            )],
            "{yaml:?}"
        );
    }
    for yaml in [
        "s: !!set {a: null, b: ~, ? c, d}\n",
        "s: !!set\n  a:\n  ? b\n  c: !!null\n",
        "n: &n ~\ns: !!set {a: *n}\n",
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(
            Validator::new().validate_syntax(file.syntax()),
            vec![],
            "{yaml:?}"
        );
    }
}

#[test]
fn violations_carry_locations() {
    for (yaml, expected) in [
        (
            "%YAML 1.2\n%YAML 1.2\n---\na\n",
            ("Duplicate %YAML directive", "2:1"),
        ),
        (
            "---\nk: v\n%YAML 1.2\n---\n",
            (
                "Directive after document requires document end marker (...)",
                "3:1",
            ),
        ),
        (
            "a: |0\n  b\n",
            (
                "Block scalar indentation indicator must be a digit 1-9",
                "1:5",
            ),
        ),
        (
            "a: \"b\" c\n",
            ("Trailing content after quoted string", "1:8"),
        ),
        (
            "a: ''[\nb: 1\n",
            ("Trailing content after quoted string", "1:6"),
        ),
        (
            "x: 1\na: b: c\n",
            (
                "Plain scalar value cannot contain mapping syntax (colon)",
                "2:5",
            ),
        ),
        (
            "k: - a\n",
            (
                "Block sequence cannot start on same line as mapping key",
                "1:4",
            ),
        ),
        (
            "k: [q]: v\n",
            (
                "Block mapping cannot start on same line as mapping key",
                "1:4",
            ),
        ),
        (
            "\u{e9}: \"a\nb\"\n",
            ("Wrong indented multiline quoted scalar", "1:4"),
        ),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        let violations: Vec<_> = Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location))
            .collect();
        assert_eq!(
            violations,
            vec![(expected.0.to_string(), Some(expected.1.to_string()))],
            "{yaml:?}"
        );
    }
}

#[test]
fn block_scalar_header_has_one_chomping_indicator() {
    let violations = |yaml: &str| -> Vec<_> {
        let file = YamlFile::from_str(yaml).unwrap();
        Validator::new()
            .validate_syntax(file.syntax())
            .into_iter()
            .map(|v| (v.message, v.location))
            .collect()
    };
    for (yaml, location) in [
        ("a: |+-\n  x\n", "1:6"),
        ("a: >++\n  x\n", "1:6"),
        ("a: |2+-\n  x\n", "1:7"),
        ("a: |+2-\n  x\n", "1:6"),
    ] {
        assert_eq!(
            violations(yaml),
            vec![(
                "Block scalar header has more than one chomping indicator".to_string(),
                Some(location.to_string())
            )],
            "{yaml:?}"
        );
    }
    for yaml in [
        "a: |+\n  x\n",
        "a: |2-\n  x\n",
        "a: |-2\n  x\n",
        "a: >+ # c\n  x\n",
    ] {
        assert_eq!(violations(yaml), vec![], "{yaml:?}");
    }
}

#[test]
fn validate_fills_locations() {
    let doc = Document::from_str("a: 1\nb: 2\na: 3\n").unwrap();
    let locations: Vec<_> = Validator::new()
        .validate(&doc)
        .into_iter()
        .map(|v| v.location)
        .collect();
    assert_eq!(locations, vec![Some("3:1".to_string())]);
}

#[test]
fn escape_needs_its_hex_digits() {
    for (yaml, expected) in [
        ("k: \"\\xZZ\"\n", "Invalid escape sequence: \\xZZ"),
        ("k: \"a\\x4z\"\n", "Invalid escape sequence: \\x4z"),
        ("k: \"\\u12\"\n", "Invalid escape sequence: \\u12\""),
        (
            "k: \"\\U0001F60\"\n",
            "Invalid escape sequence: \\U0001F60\"",
        ),
    ] {
        assert_eq!(messages(yaml), vec![expected.to_string()], "{yaml:?}");
    }
    assert_eq!(
        messages("k: \"\\u00e9\\x41\\U0001F600\"\n"),
        Vec::<String>::new()
    );
}

#[test]
fn continuation_must_clear_enclosing_entry() {
    for (yaml, expected) in [
        (
            "a:\n  b: [x,\n  y]\n",
            "Flow collection continuation line must be indented",
        ),
        (
            "a:\n  b: \"x\n  y\"\n",
            "Wrong indented multiline quoted scalar",
        ),
        (
            "- a: \"x\n  y\"\n",
            "Wrong indented multiline quoted scalar",
        ),
        (
            "- \"a\n\tb\"\n",
            "Tabs are not allowed for indentation in YAML",
        ),
        (
            "k: \"a\n\tb\"\n",
            "Tabs are not allowed for indentation in YAML",
        ),
    ] {
        assert_eq!(messages(yaml), vec![expected.to_string()], "{yaml:?}");
    }
    for yaml in [
        "a:\n  b: [x,\n   y]\n",
        "a:\n  b: \"x\n   y\"\n",
        "- a: \"x\n   y\"\n",
        "k: [a,\n# c\n  b]\n",
    ] {
        assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
    }
}

#[test]
fn blank_line_deeper_than_explicit_indent_is_content() {
    // With an indentation indicator a line indented past the body is
    // content, not a leading blank line that sets the indentation.
    for yaml in [
        "k: |2-\n   \n  x\n",
        "- >2\n   \n  x\n",
        "a:\n  k: |2\n     \n    x\n",
    ] {
        assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
    }
    assert_eq!(
            messages("k: |\n   \n  x\n"),
            vec![
                "Block scalar content under-indented (2 spaces) relative to preceding blank line (3 spaces)"
                    .to_string()
            ]
        );
}

#[test]
fn block_mapping_with_flow_collection_key() {
    // The mapping's first token is the key's `{` or `[`, which made it
    // look like a flow collection.
    for yaml in [
        "{ a: b }:\n  hr: 65\nx: y\n",
        "{ a: b }: c\nx: y\n",
        "{ a: b }:\n- 1\nx: y\n",
        "- { a: b }:\n    hr: 65\n  x: y\n",
        "k:\n  { a: b }:\n    hr: 65\n  x: y\n",
        "[ a ]:\n  hr: 65\nx: y\n",
        "[a]: 1\n[b]: 2\n",
    ] {
        assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
    }
    for (yaml, expected) in [
            (
                "{a: b}:\n  hr: 65\n x: y\n",
                "Sibling block mapping entries have inconsistent indentation (expected \"\", found \" \")",
            ),
            (
                "[a]: 1\nb: [x,\ny]\n",
                "Flow collection continuation line must be indented",
            ),
            (
                "{a: b}: 1\nb: [x,\ny]\n",
                "Flow collection continuation line must be indented",
            ),
            (
                "a:\n \t{x: y}: z\n",
                "Tabs are not allowed for indentation in YAML",
            ),
            (
                "--- {a: b}: c\n",
                "Content on same line as document start marker",
            ),
            (
                "--- [a]: c\n",
                "Content on same line as document start marker",
            ),
        ] {
            assert_eq!(messages(yaml), vec![expected.to_string()], "{yaml:?}");
        }
}

#[test]
fn tab_before_flow_collection_is_separation() {
    // Only a block collection needs `s-indent` after an indicator; a flow
    // collection is separated from it like a scalar.
    for yaml in [
        "a: b\nseq:\t[]\n",
        "a: b\nseq:\t[1]\n",
        "seq:\t{}\n",
        "k:\n  seq:\t[]\n  x: 1\n",
        "- a\n-\t[]\n",
        "-\t{a: b}\n",
        "?\t[a]\n:\t{b: c}\n",
        "seq:\t&a [x]\n",
    ] {
        assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
    }
    for yaml in ["-\t{a: b}: c\n", "-\t[a]: c\n"] {
        assert_eq!(
            messages(yaml),
            vec!["Tabs are not allowed for indentation in YAML".to_string()],
            "{yaml:?}"
        );
    }
}

#[test]
fn tab_in_root_flow_collection_is_separation() {
    // Anywhere inside a flow collection at the stream's own level a
    // leading tab is separation, as in 6CA3.
    for yaml in [
        "{a:\n\tb}\n",
        "[[\n\ta]]\n",
        "{a: [\n\tb]}\n",
        "{?\n\ta}\n",
        "[k:\n\tv]\n",
    ] {
        assert_eq!(located(yaml), vec![], "{yaml:?}");
    }
    assert_eq!(
        located("a: {b:\n\tc}\n"),
        vec![
            (
                "Flow collection continuation line must be indented".to_string(),
                "1:4".to_string()
            ),
            (
                "Tabs are not allowed for indentation in YAML".to_string(),
                "2:1".to_string()
            ),
        ]
    );
}

#[test]
fn tab_before_root_block_collection() {
    // `s-indent` is spaces only, also for the stream's own block
    // collection and an explicit key's content.
    let tab = "Tabs are not allowed for indentation in YAML";
    for (yaml, location) in [
        ("\ta: 1\n", "1:1"),
        ("\t- a\n", "1:1"),
        ("  \ta: 1\n", "1:1"),
        ("\t&x a: 1\n", "1:1"),
        ("\t{a: b}: c\n", "1:1"),
        ("---\n\ta: 1\n", "2:1"),
        ("&x\n\ta: 1\n", "2:1"),
        ("?\n\n\t?\n", "3:1"),
        ("?\n\t- a\n", "2:1"),
        ("?\n\tfoo\n", "2:1"),
    ] {
        assert_eq!(
            located(yaml),
            vec![(tab.to_string(), location.to_string())],
            "{yaml:?}"
        );
    }
    // Before a flow node, properties or on a line of its own the tab
    // is separation.
    for yaml in [
        "\tfoo\n",
        "\t[a]\n",
        "\t{a: b}\n",
        "\t&x\na: 1\n",
        "\t\n- a\n",
        "\t# c\na: 1\n",
        "?\n \tfoo\n",
    ] {
        assert_eq!(located(yaml), vec![], "{yaml:?}");
    }
}

#[test]
fn tab_before_comment_is_separation() {
    // The block collection starts on the next line, so the tab
    // separates the indicator from the comment.
    for yaml in [
        "a:\t#\n  b: c\n",
        "a:\t&x #c\n  b: c\n",
        "-\t#c\n  - x\n",
        "?\t#c\n  - x\n",
        "a: #\n  b: c\n",
        "a:\t\n  b: c\n",
    ] {
        assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
    }
}

#[test]
fn block_indicator_in_plain_scalar_continuation() {
    // A `|` or `>` opening a continuation line is plain scalar content.
    for yaml in ["a: b\n  > c\n", "a: b\n  | c\n", "- x\n  | y\n"] {
        assert_eq!(messages(yaml), Vec::<String>::new(), "{yaml:?}");
    }
}

#[test]
fn verbatim_collection_tags() {
    // `!<tag:yaml.org,2002:set>` is the tag `!!set` abbreviates.
    for (yaml, expected) in [
        (
            "a: !<tag:yaml.org,2002:set> [x]\n",
            vec!["!!set content is not a mapping"],
        ),
        (
            "a: !<tag:yaml.org,2002:set> {x: 1}\n",
            vec!["!!set member \"x\" has non-null value \"1\""],
        ),
        (
            "a: !<tag:yaml.org,2002:omap> a\n",
            vec!["!!omap content is not a sequence"],
        ),
        (
            "a: !<tag:yaml.org,2002:pairs> a\n",
            vec!["!!pairs content is not a sequence"],
        ),
        (
            "a: !<tag:yaml.org,2002:omap> [a]\n",
            vec!["!<tag:yaml.org,2002:omap> item \"a\" is not a mapping with a single entry"],
        ),
        (
            "a: !!omap [!<tag:yaml.org,2002:map> {b: 1}, {b: 2}]\n",
            vec!["Duplicate !!omap key: \"b\" (semantically equal to \"b\")"],
        ),
        (
            "a: !<tag:yaml.org,2002:omap> [{b: 1}, {b: 2}]\n",
            vec!["Duplicate !!omap key: \"b\" (semantically equal to \"b\")"],
        ),
        ("a: !!pairs [!<tag:yaml.org,2002:map> {b: 1}]\n", vec![]),
    ] {
        assert_eq!(messages(yaml), expected, "{yaml:?}");
    }
}

/// What `Validator::validate` reports about each document.
fn document_violations(yaml: &str) -> Vec<(String, Rule)> {
    let doc = Document::from_str(yaml).unwrap();
    Validator::new()
        .validate(&doc)
        .into_iter()
        .map(|v| (v.message, v.rule))
        .collect()
}

#[test]
fn duplicate_keys() {
    // Keys that differ textually but mean the same thing are duplicates;
    // bucketing them by normalized value must not lose these. Keys of
    // different types are not.
    for (yaml, expected) in [
        ("key: value", None),
        ("a: 1\nb: 2\n", None),
        ("a: 1\nb: 2\nc: 3\n", None),
        (
            "a: 1\nb: 2\na: 3\n",
            Some("Duplicate key: \"a\" (semantically equal to \"a\")"),
        ),
        (
            "a: 1\n\"a\": 2\n",
            Some("Duplicate key: \"\\\"a\\\"\" (semantically equal to \"a\")"),
        ),
        (
            "'a': 1\na: 2",
            Some("Duplicate key: \"a\" (semantically equal to \"'a'\")"),
        ),
        (
            "1: a\n0x1: b\n",
            Some("Duplicate key: \"0x1\" (semantically equal to \"1\")"),
        ),
        (
            "1: one\n0x1: hex",
            Some("Duplicate key: \"0x1\" (semantically equal to \"1\")"),
        ),
        (
            "true: 1\nTrue: 2",
            Some("Duplicate key: \"True\" (semantically equal to \"true\")"),
        ),
        (
            "n: 1\nnull: 2\n~: 3\n",
            Some("Duplicate key: \"~\" (semantically equal to \"null\")"),
        ),
        (
            "null: 1\n~: 2",
            Some("Duplicate key: \"~\" (semantically equal to \"null\")"),
        ),
        // An empty key is null, which is not the empty string.
        (
            ": 1\n: 2\n",
            Some("Duplicate key: null (semantically equal to null)"),
        ),
        (
            ": 1\n~: 2\n",
            Some("Duplicate key: \"~\" (semantically equal to null)"),
        ),
        (
            "\"\": 1\n'': 2\n",
            Some("Duplicate key: \"''\" (semantically equal to \"\\\"\\\"\")"),
        ),
        (": 1\n\"\": 2\n", None),
        (
            "[1,2]: a\n[1, 2]: b\n",
            Some("Duplicate key: \"[1, 2]\" (semantically equal to \"[1,2]\")"),
        ),
        (
            "a: 1\n!!str a: 2\n",
            Some("Duplicate key: \"!!str a\" (semantically equal to \"a\")"),
        ),
        (
            "1: a\n!!int 0x1: b\n",
            Some("Duplicate key: \"!!int 0x1\" (semantically equal to \"1\")"),
        ),
        (
            "? [1]\n: a\n? !!seq [1]\n: b\n",
            Some("Duplicate key: \"!!seq [1]\" (semantically equal to \"[1]\")"),
        ),
        ("1: a\n'1': b\n", None),
        ("1: a\n!!str 1: b\n", None),
        ("\"1\": string\n1: int", None),
        ("1.0: float\n1: int", None),
    ] {
        let expected: Vec<_> = expected
            .map(|m| (m.to_string(), Rule::DuplicateKeys))
            .into_iter()
            .collect();
        assert_eq!(document_violations(yaml), expected, "{yaml:?}");
    }
}

#[test]
fn duplicate_key_check_can_be_disabled() {
    let validator = Validator::with_config(ValidatorConfig {
        check_duplicate_keys: false,
        ..ValidatorConfig::default()
    });
    for yaml in ["a: 1\nb: 2\na: 3\n", "key: value"] {
        let doc = Document::from_str(yaml).unwrap();
        assert_eq!(validator.validate(&doc), vec![], "{yaml:?}");
    }
}

#[test]
fn document_violations_by_rule() {
    let tab = "Tabs are not allowed for indentation in YAML";
    let anchors = "Multiple anchors on the same node";
    for (yaml, expected) in [
        (
            "---\na:\n\tb:\n\t\tc: value",
            vec![(tab, Rule::InvalidTabUsage), (tab, Rule::InvalidTabUsage)],
        ),
        (
            "\"\\.\"\n",
            vec![("Invalid escape sequence: \\.", Rule::InvalidEscape)],
        ),
        ("&a &b key: value", vec![(anchors, Rule::InvalidAnchor)]),
        // Only the second entry really carries two anchors on one node: in
        // the first, `&node1` anchors the nested mapping and `&k1` its key,
        // which is why the test suite calls the same shape valid in 7BMT
        // (this is 4JVG).
        (
            "top1: &node1\n  &k1 key1: val1\ntop2: &node2\n  &v2 val2\n",
            vec![(anchors, Rule::InvalidAnchor)],
        ),
    ] {
        let expected: Vec<_> = expected
            .into_iter()
            .map(|(m, r)| (m.to_string(), r))
            .collect();
        assert_eq!(document_violations(yaml), expected, "{yaml:?}");
    }
}

#[test]
fn tabs_stay_in_the_tree() {
    let doc = Document::from_str("---\na:\n\tb:\n\t\tc: value").unwrap();
    assert!(doc
        .syntax()
        .descendants_with_tokens()
        .any(|e| e.as_token().is_some_and(|t| t.text().contains('\t'))));
}

#[test]
fn located_violations() {
    for (yaml, message, location) in [
        // 9MMA: a directive without any document.
        (
            "%YAML 1.2\n",
            "Directive without document start marker (---)",
            "1:1",
        ),
        (
            "key: \"value\"# invalid comment\n",
            "Comment without whitespace separation",
            "1:13",
        ),
        (
            "--- key1: value1\n    key2: value2\n",
            "Content on same line as document start marker",
            "1:1",
        ),
    ] {
        assert_eq!(
            located(yaml),
            vec![(message.to_string(), location.to_string())],
            "{yaml:?}"
        );
    }
    // A byte order mark may directly precede a comment line.
    assert_eq!(located("\u{FEFF}# c\na: 1\n"), vec![]);
}

/// What the validator reports about `yaml`: message, location, severity
/// and rule of each violation.
fn findings(yaml: &str) -> Vec<(String, String, Severity, Rule)> {
    let file = YamlFile::from_str(yaml).unwrap();
    Validator::new()
        .validate_syntax(file.syntax())
        .into_iter()
        .map(|v| (v.message, v.location.unwrap(), v.severity, v.rule))
        .collect()
}

/// A merge key operand has to be a mapping or a sequence of mappings, or
/// an alias to one. The validator warns about the operands that
/// `YamlValue` cannot merge, and about no others.
#[test]
fn merge_key_operands() {
    let warning = |operand: &str, location: &str| {
        vec![(
            format!("Merge key operand {operand:?} is not a mapping"),
            location.to_string(),
            Severity::Warning,
            Rule::Other,
        )]
    };
    for (yaml, expected) in [
        ("<<: 1\nb: 2\n", warning("1", "1:5")),
        ("m: {<<: 1}\n", warning("1", "1:9")),
        ("<<: [1]\n", warning("1", "1:6")),
        ("<<: [{a: 1}, x]\n", warning("x", "1:14")),
        ("<<: !!seq [{a: 1}, x]\n", warning("x", "1:20")),
        ("x: &x 1\nm:\n  <<: *x\n", warning("*x", "3:7")),
        // An operand of a sequence anchored elsewhere is reported where
        // that sequence is merged in.
        ("s: &s [{a: 1}, 1]\nm:\n  <<: *s\n", warning("1", "3:7")),
        ("a: &a {x: 1}\nb:\n  <<: *a\n", vec![]),
        ("<<: [{a: 1}, {b: 2}]\n", vec![]),
        ("<<: !!map {a: 1}\n", vec![]),
        ("s: &s [{a: 1}]\nm:\n  <<: *s\n", vec![]),
        ("'<<': 1\n", vec![]),
        ("\"<<\": [1]\n", vec![]),
    ] {
        assert_eq!(findings(yaml), expected, "{yaml:?}");
        let converted = yaml_edit::YamlValue::parse_raw(yaml);
        assert_eq!(
            converted.is_ok(),
            expected.is_empty(),
            "{yaml:?}: {converted:?}"
        );
    }
}
