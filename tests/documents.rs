//! Document markers, directives and streams of several documents.

mod common;
use common::{
    assert_cst_ok, assert_file_cst_ok, assert_file_edited, assert_file_edited_valid,
    assert_file_parser_shaped, parse_clean, render, render_documents,
};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{
    debug, Directive, Document, Mapping, MappingBuilder, SyntaxKind, YamlError, YamlFile, YamlNode,
    YamlValue,
};

#[test]
fn document_streams() {
    for (yaml, documents) in [
        ("---\na: 1\n", &["map{a: 1}"][..]),
        ("--- a\n", &["a"]),
        (
            "---\ndoc1: first\n---\ndoc2: second\n...\n",
            &["map{doc1: first}", "map{doc2: second}"],
        ),
        ("---\nkey: value\n...\n", &["map{key: value}"]),
        ("key: value\n...\n", &["map{key: value}"]),
        (
            "%YAML 1.2\n%TAG ! tag:example.com,2000:app/\n---\nfirst: doc\n...\n%YAML 1.2\n---\nsecond: doc\n...\n",
            &["map{first: doc}", "map{second: doc}"],
        ),
        (
            "---\nkey1: value1\nkey2: value2",
            &["map{key1: value1, key2: value2}"],
        ),
        (
            "---\njson: {a: 1, b: 2}\n---\nyaml:\n  nested: value\n",
            &["map{json: {a: 1, b: 2}}", "map{yaml: map{nested: value}}"],
        ),
        (
            "---\n---\n---\nkey: value\n",
            &["~", "~", "map{key: value}"],
        ),
        ("%YAML 1.2\n---\nkey: value\n", &["map{key: value}"]),
        (
            "---\ndoc: first\n---\ndoc: second\n...\n",
            &["map{doc: first}", "map{doc: second}"],
        ),
        (
            "---\n---\nkey: value\n---\n...\n",
            &["~", "map{key: value}", "~"],
        ),
        (
            "---\nfirst: doc\n...\n---\nsecond: doc\n---\nthird: doc\n...\n",
            &["map{first: doc}", "map{second: doc}", "map{third: doc}"],
        ),
        (
            "\n%YAML 1.2\n---\ndoc1: value1\n...\n---\ndoc2: value2\n---\ndoc3: value3\n...\n",
            &[
                "map{doc1: value1}",
                "map{doc2: value2}",
                "map{doc3: value3}",
            ],
        ),
    ] {
        assert_eq!(render_documents(yaml), documents, "{yaml:?}");
    }
}

#[test]
fn large_document_stream() {
    let yaml: String = (0..100)
        .map(|i| format!("---\ndoc{i}: value{i}\n"))
        .collect();
    let expected: Vec<String> = (0..100)
        .map(|i| format!("map{{doc{i}: value{i}}}"))
        .collect();
    assert_eq!(render_documents(&yaml), expected);
}

/// A `...` document-end marker belongs to the tree, and may be followed by
/// a fresh document with or without a `---` of its own.
///
/// The trailing sweep stopped only at `---` and a directive, so an explicit
/// end marker after a `---` document was swept away as a stray token, and
/// the content after a marker (suite cases 7Z25, M7A3) was lost to an ERROR
/// node while from_str still reported success. A `...` with no content
/// before it ends no document (suite cases HWV9, QT73).
#[test]
fn document_end_marker() {
    for (yaml, documents) in [
        ("--- a\n...\n", &["a"][..]),
        ("--- a\n...\n--- b\n", &["a", "b"]),
        ("--- a\n...\n--- b\n...\n", &["a", "b"]),
        ("a: 1\n...\n", &["map{a: 1}"]),
        ("a\n...\nb: 1\n", &["a", "map{b: 1}"]),
        ("--- a\n...\nkey: value\n", &["a", "map{key: value}"]),
        ("...\n---\na\n", &["a"]),
        ("...\n", &[]),
        ("# comment\n...\n", &[]),
        ("...\n...\n", &[]),
        // A comment is not content, so one on the marker's line is fine.
        // It was swept into an ERROR node with no error reported.
        ("... #", &[]),
        ("... # done\n", &[]),
        ("a\n... # done\n", &["a"]),
        ("a: 1\n...  #\n", &["map{a: 1}"]),
    ] {
        assert_eq!(render_documents(yaml), documents, "{yaml:?}");
    }
}

/// Real content after a `...` on its line is an error.
#[test]
fn content_after_document_end_marker() {
    for (yaml, error, from_str_error) in [
        (
            "... x\n",
            "1:5: Content after the document end marker: \"x\"",
            "Parse error at line 1, column 5: Content after the document end marker: \"x\"",
        ),
        (
            "a\n... x\n",
            "2:5: Content after the document end marker: \"x\"",
            "Parse error at line 2, column 5: Content after the document end marker: \"x\"",
        ),
    ] {
        let parsed = YamlFile::parse(yaml);
        assert_eq!(parsed.errors(), [error], "{yaml:?}");
        assert_eq!(parsed.tree().to_string(), yaml);
        let err = YamlFile::from_str(yaml).unwrap_err();
        assert_eq!(err.to_string(), from_str_error, "{yaml:?}");
    }
}

/// A `---` or `...` marker ends at a space, a tab or a line break, which is
/// all YAML 1.2 counts as whitespace there.
///
/// Rust's char::is_whitespace is Unicode-wide, so a no-break space after
/// the dots looked like the end of a marker. `"...\u{a0}-"` is one plain
/// scalar, as saphyr and PyYAML both read it, but it parsed as a document
/// end with the rest stranded in an ERROR node and no error reported.
#[test]
fn unicode_space_does_not_end_a_document_marker() {
    for yaml in ["...\u{a0}-", "---\u{a0}x", "...\u{2028}a", "---\u{00a0}"] {
        assert_eq!(render_documents(yaml), [yaml], "{yaml:?}");
    }
}

/// A document marker inside a quoted scalar is a spec violation (test
/// suite 5TRB, RXY3) that the validator reports; the parser keeps it as
/// part of the scalar.
#[test]
fn document_marker_inside_a_quoted_scalar() {
    for (yaml, violations) in [
        (
            "a: \"x\n---\ny\"\n",
            &[("Document marker inside a quoted scalar", "1:4")][..],
        ),
        (
            "'x\n... y'\n",
            &[("Document marker inside a quoted scalar", "1:1")],
        ),
        // Without whitespace after it, `---` is no marker.
        ("\"x\n---y\"\n", &[]),
    ] {
        common::parse_with_violations(yaml, violations);
    }
}

/// A `...` that ends a document belongs to that DOCUMENT node whatever the
/// document's root is; only one with no document before it sits at the root.
#[test]
fn document_end_marker_is_inside_the_document_it_ends() {
    for (yaml, inside) in [
        ("a\n...\n", true),
        ("[a]\n...\n", true),
        ("\"q\"\n# c\n...\n", true),
        ("&x a\n...\n", true),
        ("a: 1\n...\n", true),
        ("--- \n...\n", true),
        ("--- a\n\n...\n--- b\n...\n", true),
        ("...\n", false),
        ("# c\n...\n", false),
    ] {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        let parents: Vec<_> = file
            .syntax()
            .descendants_with_tokens()
            .filter(|e| e.kind() == SyntaxKind::DOC_END)
            .map(|e| e.parent().unwrap().kind())
            .collect();
        let expected = if inside {
            SyntaxKind::DOCUMENT
        } else {
            SyntaxKind::ROOT
        };
        assert!(!parents.is_empty(), "{yaml:?}");
        assert!(
            parents.iter().all(|k| *k == expected),
            "{yaml:?}: {parents:?}"
        );
    }
}

/// What ends a document must not change the shape of its last line: `---`
/// and `...` are not content for a lone annotation to adopt, nor a dedent
/// that moves a comment line out of the collection it is indented under.
#[test]
fn document_shape_does_not_depend_on_what_follows() {
    let first_document = |yaml: &str| {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        debug::tree_to_string(file.document().unwrap().syntax())
    };
    for src in [
        "!!str\n",
        "&a\n",
        "a: !!str\n",
        "- &a\n",
        "a:\n  b: 1\n  # c\n",
        "a:\n  - 1\n  # c\n",
        "a:\n  b: 1\n# c\n",
        "a:\n  b:\n    c: 1\n    # c\n",
    ] {
        let expected = first_document(src);
        for end in ["---\nb: 2\n", "--- b\n"] {
            assert_eq!(
                first_document(&format!("{src}{end}")),
                expected,
                "{src:?} before {end:?}"
            );
        }
        // `...` is part of the document it ends.
        assert_eq!(
            first_document(&format!("{src}...\n")),
            format!("{expected}DOC_END: \"...\"\nNEWLINE: \"\\\\n\"\n"),
            "{src:?} before \"...\""
        );
    }
}

#[test]
fn ensure_document_after_lone_document_end_marker() {
    let parsed = YamlFile::from_str("# comment\n...\n").unwrap();
    parsed.ensure_document().set("key", "value");
    assert_eq!(parsed.to_string(), "# comment\n...\nkey: value\n");
    let reparsed = YamlFile::from_str(&parsed.to_string()).unwrap();
    assert_eq!(reparsed.documents().count(), 1);
}

/// A block collection may not start on the `---` line, which the validator
/// reports, but `--- - 1` still has to read as the sequence it looks like,
/// just as `--- a: 1` reads as a mapping. It used to be the plain scalar
/// `- 1`, and `--- - 1\n- 2\n` the scalar `- 1 - 2`.
#[test]
fn block_sequence_on_document_start_line() {
    let text = "--- - 1\n- 2\n";
    let file = YamlFile::from_str(text).unwrap();
    assert_file_cst_ok(&file);
    assert_eq!(
        debug::tree_to_string(file.syntax()),
        concat!(
            "DOCUMENT\n",
            "  DOC_START: \"---\"\n",
            "  WHITESPACE: \" \"\n",
            "  SEQUENCE\n",
            "    SEQUENCE_ENTRY\n",
            "      DASH: \"-\"\n",
            "      WHITESPACE: \" \"\n",
            "      SCALAR\n",
            "        INT: \"1\"\n",
            "      NEWLINE: \"\\\\n\"\n",
            "    SEQUENCE_ENTRY\n",
            "      DASH: \"-\"\n",
            "      WHITESPACE: \" \"\n",
            "      SCALAR\n",
            "        INT: \"2\"\n",
            "      NEWLINE: \"\\\\n\"\n",
        )
    );
    let doc = file.document().unwrap();
    let seq = doc.as_sequence().unwrap();
    assert_eq!(seq.len(), 2);
    assert_eq!(
        YamlValue::from_document(&doc).unwrap(),
        YamlValue::from(vec![1i64, 2])
    );
    assert_eq!(file.to_string(), text);

    let file = YamlFile::from_str("--- -\n").unwrap();
    let seq = file.document().unwrap().as_sequence().unwrap();
    assert_eq!(seq.len(), 1);
}

/// A directive at the start of a document is still a directive.
#[test]
fn document_directives() {
    for (yaml, documents) in [
        ("%YAML 1.2\n---\na: 1\n", &["map{a: 1}"][..]),
        ("%TAG ! x\n---\na\n", &["a"]),
        ("%YAML 1.2\n---\n", &["~"]),
    ] {
        assert_eq!(render_documents(yaml), documents, "{yaml:?}");
        let file = parse_clean(yaml);
        assert_eq!(common::count_nodes(&file, SyntaxKind::DIRECTIVE), 1);
    }
}

fn doc_values(yaml: &str) -> Vec<Option<i64>> {
    YamlFile::from_str(yaml)
        .unwrap()
        .documents()
        .map(|d| {
            let m = d.as_mapping().unwrap();
            let key = m.keys().next().unwrap();
            m.get(key).and_then(|v| v.to_i64())
        })
        .collect()
}

/// Pushing a document separates it from the one before with `---`, and
/// ends the line before it if that has no line break.
#[test]
fn push_document() {
    let new_mapping = || {
        let doc = Document::new_mapping();
        doc.set("b", 2);
        assert_cst_ok(&doc);
        doc
    };
    let built = || MappingBuilder::new().pair("b", 2).build_document();
    let parsed = |text: &str| Document::from_str(text).unwrap();
    for (src, pushed, expected, values) in [
        (
            "a: 1\n",
            new_mapping(),
            "a: 1\n---\nb: 2\n",
            vec![Some(1), Some(2)],
        ),
        (
            "a: 1\n",
            built(),
            "a: 1\n---\nb: 2\n",
            vec![Some(1), Some(2)],
        ),
        (
            "a: 1",
            parsed("b: 2"),
            "a: 1\n---\nb: 2",
            vec![Some(1), Some(2)],
        ),
        (
            "a: 1\n",
            parsed("---\nb: 2\n"),
            "a: 1\n---\nb: 2\n",
            vec![Some(1), Some(2)],
        ),
        (
            "a: 1\n# end",
            parsed("b: 2\n"),
            "a: 1\n# end\n---\nb: 2\n",
            vec![Some(1), Some(2)],
        ),
        (
            "a:\n",
            parsed("b: 2\n"),
            "a:\n---\nb: 2\n",
            vec![None, Some(2)],
        ),
    ] {
        let yaml = YamlFile::from_str(src).unwrap();
        yaml.push_document(pushed);
        assert_file_edited(&yaml, expected);
        assert_eq!(doc_values(expected), values, "{src:?}");
    }
}

/// Comments before a document without `---` are part of it; those
/// before a `---`, a directive or no document are the stream's.
#[test]
fn leading_comment_placement() {
    let tree =
        |src: &str| yaml_edit::debug::tree_to_string(YamlFile::from_str(src).unwrap().syntax());
    assert_eq!(
        tree("# c\n\nfoo\n"),
        "\
DOCUMENT
  COMMENT: \"# c\"
  NEWLINE: \"\\\\n\"
  NEWLINE: \"\\\\n\"
  SCALAR
    STRING: \"foo\"
  NEWLINE: \"\\\\n\"
"
    );
    assert_eq!(
        tree("# c\n--- foo\n"),
        "\
COMMENT: \"# c\"
NEWLINE: \"\\\\n\"
DOCUMENT
  DOC_START: \"---\"
  WHITESPACE: \" \"
  SCALAR
    STRING: \"foo\"
  NEWLINE: \"\\\\n\"
"
    );
    assert_eq!(
        tree("# c\n%YAML 1.2\n--- foo\n"),
        "\
COMMENT: \"# c\"
NEWLINE: \"\\\\n\"
DIRECTIVE
  DIRECTIVE: \"%YAML 1.2\"
NEWLINE: \"\\\\n\"
DOCUMENT
  DOC_START: \"---\"
  WHITESPACE: \" \"
  SCALAR
    STRING: \"foo\"
  NEWLINE: \"\\\\n\"
"
    );
    assert_eq!(
        tree("# c\n"),
        "\
COMMENT: \"# c\"
NEWLINE: \"\\\\n\"
"
    );
    assert_eq!(
        tree("a\n...\n# c\nb\n"),
        "\
DOCUMENT
  SCALAR
    STRING: \"a\"
  NEWLINE: \"\\\\n\"
  DOC_END: \"...\"
  NEWLINE: \"\\\\n\"
COMMENT: \"# c\"
NEWLINE: \"\\\\n\"
DOCUMENT
  SCALAR
    STRING: \"b\"
  NEWLINE: \"\\\\n\"
"
    );
}

#[test]
fn add_directive_keeps_bom_first() {
    for (src, expected) in [
        ("\u{feff}a: 1\n", "\u{feff}%YAML 1.2\n---\na: 1\n"),
        ("\u{feff}--- a\n", "\u{feff}%YAML 1.2\n--- a\n"),
        ("\u{feff}# c\na: 1\n", "\u{feff}%YAML 1.2\n---\n# c\na: 1\n"),
        ("\u{feff}", "\u{feff}%YAML 1.2\n"),
    ] {
        let yaml = YamlFile::from_str(src).unwrap();
        yaml.add_directive("%YAML 1.2");
        assert_eq!(yaml.to_string(), expected);
        assert_file_parser_shaped(&yaml);
    }
}

#[test]
fn push_document_keeps_bom_before_start_marker() {
    for (src, pushed, expected) in [
        ("a: 1\n", "\u{feff}b: 2\n", "a: 1\n\u{feff}---\nb: 2\n"),
        ("a: 1\n", "\u{feff}--- b\n", "a: 1\n\u{feff}--- b\n"),
        (
            "a: 1\n...\n",
            "\u{feff}b: 2\n",
            "a: 1\n...\n\u{feff}---\nb: 2\n",
        ),
        (
            "foo\n...\n",
            "\u{feff}b: 2\n",
            "foo\n...\n\u{feff}---\nb: 2\n",
        ),
        ("", "\u{feff}b: 2\n", "\u{feff}b: 2\n"),
        (
            "%YAML 1.2\n",
            "\u{feff}b: 2\n",
            "\u{feff}%YAML 1.2\n---\nb: 2\n",
        ),
        (
            "\u{feff}%YAML 1.2\n",
            "\u{feff}b: 2\n",
            "\u{feff}%YAML 1.2\n---\nb: 2\n",
        ),
        ("# c\n", "\u{feff}b: 2\n", "\u{feff}# c\nb: 2\n"),
    ] {
        let yaml = YamlFile::from_str(src).unwrap();
        yaml.push_document(Document::from_str(pushed).unwrap());
        assert_eq!(yaml.to_string(), expected, "{src:?} + {pushed:?}");
        assert_file_parser_shaped(&yaml);
    }
}

#[test]
fn push_document_terminates_last_line_like_parser() {
    for src in [
        "a: 1",
        "- 1",
        "a: 1\n# end",
        "- 1\n# end",
        "foo",
        "foo # c",
        "{a: 1}",
        "|\n x",
        "!!str",
        "&a",
        "!!str # c",
        "--- !!str",
        "---",
        "--- # c",
        "a: [1]",
        "a: 1 # c",
        "a: |\n  x",
        "a:",
        "a: &x",
        "a: # c",
        "a:\n  b: 1",
        "a:\n  - 1",
        "a:\n  b:",
        "a: !t\n  b: 1",
        "? a",
        "? a\n: b",
        "? a\n:",
        "? a\n: # c",
        "? a\n: - 1",
        "? a\n: {b: 1}",
        "-",
        "- a: 1",
        "- - 1",
        "- |\n  x",
        "- # c",
        "- &a # c",
        "- \n  # c",
        "- a # c",
        "- ? a\n  : b",
        "- ? a\n  :",
        "a: !!str",
        "- &a",
        "a:\n  b: 1\n  # c",
        "a:\n  - 1\n  # c",
    ] {
        let yaml = YamlFile::from_str(src).unwrap();
        yaml.push_document(Document::from_str("b: 2\n").unwrap());
        assert_eq!(yaml.to_string(), format!("{src}\n---\nb: 2\n"));
        assert_file_parser_shaped(&yaml);
    }
}

/// The lines before the first document are its own, except for comments
/// before its `---`.
#[test]
fn push_first_document_after_lines() {
    for (src, pushed, expected) in [
        ("# c\n", "b: 2\n", "# c\nb: 2\n"),
        ("\n# c\n\n", "- 1\n", "\n# c\n\n- 1\n"),
        ("# c\n", "--- b\n", "# c\n--- b\n"),
        ("\n\n", "--- b\n", "\n\n--- b\n"),
        ("# c", "b: 2\n", "# c\nb: 2\n"),
    ] {
        let yaml = YamlFile::from_str(src).unwrap();
        yaml.push_document(Document::from_str(pushed).unwrap());
        assert_eq!(yaml.to_string(), expected, "{src:?} + {pushed:?}");
        assert_file_parser_shaped(&yaml);
    }
}

#[test]
fn push_document_into_empty_file() {
    let yaml = YamlFile::new();
    yaml.push_document(Document::from_str("b: 2\n").unwrap());
    assert_file_edited(&yaml, "b: 2\n");
}

/// A snapshot is unaffected by later edits, which tree_eq notices.
#[test]
fn snapshot_and_tree_eq() {
    let file = YamlFile::from_str("a: 1\n").unwrap();
    let snapshot = file.snapshot();
    assert!(file.tree_eq(&snapshot));
    assert!(file.tree_eq(&YamlFile::from_str("a: 1\n").unwrap()));
    file.set("a", 2);
    assert!(!file.tree_eq(&snapshot));
    assert_eq!(snapshot.to_string(), "a: 1\n");
    assert_eq!(YamlFile::default().to_string(), "");
}

#[test]
fn read_relaxed_and_from_path() {
    let (file, errors) = YamlFile::read_relaxed("a: [\n".as_bytes()).unwrap();
    assert_eq!(file.to_string(), "a: [\n");
    assert_eq!(
            errors,
            ["2:1: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the array, or check for missing commas between elements"]
        );

    let path = std::env::temp_dir().join(format!("yaml-edit-file-{}.yaml", std::process::id()));
    std::fs::write(&path, "a: [\n").unwrap();
    let (file, errors) = YamlFile::from_path_relaxed(&path).unwrap();
    assert_eq!(file.to_string(), "a: [\n");
    assert_eq!(errors.len(), 1);
    assert!(matches!(
        YamlFile::from_path(&path),
        Err(yaml_edit::YamlError::Parse {
            line: Some(2),
            column: Some(1),
            ..
        })
    ));
    std::fs::write(&path, "a: 1\n").unwrap();
    assert_eq!(YamlFile::from_path(&path).unwrap().to_string(), "a: 1\n");
    std::fs::remove_file(&path).unwrap();
    assert_eq!(
        YamlFile::from_path_relaxed(&path).unwrap_err().kind(),
        std::io::ErrorKind::NotFound
    );
    assert!(matches!(
        YamlFile::from_path(&path),
        Err(yaml_edit::YamlError::Io(_))
    ));
}

#[test]
fn set_and_insert_add_a_missing_document() {
    for (src, expected) in [
        ("", "k: v\n"),
        ("# c\n", "# c\nk: v\n"),
        ("%YAML 1.2\n", "%YAML 1.2\n---\nk: v\n"),
    ] {
        let set = |file: &YamlFile| file.set("k", "v");
        let insert = |file: &YamlFile| file.insert_at_index(0, "k", "v");
        for add in [&set as &dyn Fn(&YamlFile) -> bool, &insert] {
            let file = YamlFile::from_str(src).unwrap();
            assert!(add(&file), "{src:?}");
            assert_file_edited(&file, expected);
        }
    }
}

/// `from_str_relaxed` keeps the tree of input with errors usable.
#[test]
fn from_str_relaxed() {
    let unclosed_sequence = "Unclosed flow sequence. Expected: ']' to close sequence. \
        Found: end of input. Context: in flow sequence. Suggestion: Add ']' to close the \
        array, or check for missing commas between elements";
    let unclosed_mapping = "Unclosed flow mapping. Expected: '}' to close mapping. \
        Found: end of input. Context: in flow mapping. Suggestion: Add '}' to close the \
        object, or check for missing commas between key-value pairs";
    for (input, errors, expected) in [
        ("key: value\n", vec![], &["map{key: value}"][..]),
        (
            "key: [a, b",
            vec![format!("1:11: {unclosed_sequence}")],
            &["map{key: [a, b]}"],
        ),
        (
            "key: {a: 1",
            vec![format!("1:11: {unclosed_mapping}")],
            &["map{key: {a: 1}}"],
        ),
        (
            "good: value\nbad: [unclosed\nalso_good: 42\n",
            vec![format!("4:1: {unclosed_sequence}")],
            &["map{good: value, bad: [{unclosed also_good: 42}]}"],
        ),
        ("", vec![], &[]),
    ] {
        let (file, found) = YamlFile::from_str_relaxed(input);
        assert_eq!(found, errors, "{input:?}");
        assert_eq!(file.to_string(), input);
        let rendered: Vec<_> = file
            .documents()
            .map(|doc| {
                render(
                    &doc.syntax()
                        .children()
                        .find_map(YamlNode::from_syntax)
                        .unwrap(),
                )
            })
            .collect();
        assert_eq!(rendered, expected, "{input:?}");
    }
}

#[test]
fn complex_document_stream() {
    let yaml = r#"%YAML 1.2
%TAG ! tag:example.com,2000:app/
---
template: &anchor
  key: !custom value
instance:
  <<: *anchor
  extra: data
...
%YAML 1.2
---
- item1
- item2: nested
...
---
literal: |
  Block content
  Multiple lines
folded: >
  Folded content
  on multiple lines
...
"#;
    let parsed = YamlFile::from_str(yaml).unwrap();
    assert_eq!(parsed.documents().count(), 3);
    assert_eq!(parsed.to_string(), yaml);
}

#[test]
fn edits_write_cr_line_breaks() {
    let file = YamlFile::from_str("a:\r- 1\r").unwrap();
    let doc = file.document().unwrap();
    doc.as_mapping()
        .unwrap()
        .get_sequence("a")
        .unwrap()
        .push("b");
    file.add_directive("%YAML 1.2");
    file.push_document(Document::from_str("d: 4\n").unwrap());
    assert_file_edited_valid(&file, "%YAML 1.2\r---\ra:\r- 1\r- b\r---\rd: 4\r");
}

/// What `Document::from_str` refuses: content that belongs to the stream
/// rather than to one document, which it would have to drop. A scalar
/// cannot follow the document's own node, which PyYAML rejects too, so
/// there is nothing to attach it to; the parser sweeps it into a
/// stream-level ERROR node that used to vanish from the output.
#[test]
fn document_from_str_refuses_stream_level_content() {
    let parse = |message: &str| YamlError::Parse {
        message: message.to_string(),
        line: Some(2),
        column: Some(1),
    };
    let refused = |reason: &str| YamlError::InvalidOperation {
        operation: "Document::from_str".to_string(),
        reason: reason.to_string(),
    };
    let comments = "Input contains stream-level comments outside the document. \
        Use YamlFile::from_str() to preserve them.";
    for (yaml, expected) in [
        (
            "- a\nx\n",
            parse("Content after the document could not be parsed: \"x\""),
        ),
        (
            "[a]\n[b]\n",
            parse("Content after the document could not be parsed: \"[\""),
        ),
        // A comment before `---` is part of the stream, not the document.
        ("# leading comment\n---\nkey: value\n", refused(comments)),
        ("\n# c\n---\na: 1\n", refused(comments)),
        ("\n# c\n", refused(comments)),
        // So is a comment after the `...` end marker.
        ("key: value\n...\n# trailing\n", refused(comments)),
        (
            "%YAML 1.2\n---\nkey: value\n",
            refused(
                "Input contains YAML directives outside the document. \
                 Use YamlFile::from_str() to preserve them.",
            ),
        ),
        (
            "---\nkey: value\n---\nother: data",
            refused(
                "Input contains multiple YAML documents. \
                 Use YamlFile::from_str() for multi-document YAML.",
            ),
        ),
        // A stream holding no document but a `...` cannot be a Document
        // without dropping the marker.
        (
            "...\n",
            refused(
                "Input contains stream-level content outside a document: \"...\". \
                 Use YamlFile::from_str() to preserve it.",
            ),
        ),
    ] {
        assert_eq!(
            format!("{:?}", Document::from_str(yaml).unwrap_err()),
            format!("{expected:?}"),
            "{yaml:?}"
        );
    }
}

/// Everything `Document::from_str` accepts round trips: the lines before
/// the root node, those after it up to the end of the document, and the
/// rest of a `...` line. Comments before a document without `---` are part
/// of it.
#[test]
fn document_from_str_round_trips() {
    for yaml in [
        "a: 1\n",
        "a\nJ",
        "- a\n- b\n",
        "key: value",
        // An anchor annotates the node on the following line, so
        // `&b\nx: 1\n` is the mapping `{x: 1}`, as saphyr and PyYAML both
        // read it.
        "&b\nJ",
        "&b\nx: 1\n",
        "&b\n- a\n",
        // A mapping after a scalar or sequence is a second node of the
        // document, which the validator reports.
        "a\nx: 1\n",
        "- a\nx: 1\n",
        "\nfoo\n",
        "\n\na: 1\n",
        "  \na: 1\n",
        "  a: 1\n",
        "\n  a: 1\n  b: 2\n",
        "\n---\na: 1\n",
        "\n[1]\n",
        "\u{feff}a: 1\n",
        "\u{feff}\n--- a\n",
        "",
        "\n",
        "\u{feff}",
        "# leading comment\nkey: value\n",
        "\n# c\na: 1\n",
        "\u{feff}# c\n\n- 1\n",
        "# c\nfoo\n",
        "{a: 1, b: 2}\n",
        "[1, 2]\n",
        "foo\n",
        "\"foo\"\n",
        "'foo'\n",
        "&x {a: 1}\n",
        "- 1\n",
        "|\n  x\n",
        "{a: 1}\n\n",
        "foo # c\n",
        "[1]\n# c\n",
        "--- foo\n",
        "{a: 1}",
        "a: 1\n...\n",
        "a: 1\n... # c\n",
        "a: 1\n...  \n",
        "- 1\n...\n",
        "---\na: 1\n...\n",
        "key: value # trailing\n",
    ] {
        let doc = Document::from_str(yaml).unwrap();
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), yaml, "{yaml:?}");
    }
}

#[test]
fn document_from_str_values() {
    for (yaml, expected) in [
        (
            "key: value\nport: 8080",
            &["map{key: value, port: 8080}"][..],
        ),
        ("", &[]),
        (
            "name: test\nversion: 1.0",
            &["map{name: test, version: 1.0}"],
        ),
        ("---\nkey: value", &["map{key: value}"]),
        (
            r#"
database:
  host: localhost
  port: 5432
  credentials:
    username: admin
    password: secret
features:
  - auth
  - logging
  - metrics
"#,
            &["map{database: map{host: localhost, port: 5432, \
               credentials: map{username: admin, password: secret}}, \
               features: seq[auth, logging, metrics]}"],
        ),
    ] {
        assert_eq!(Document::from_str(yaml).unwrap().to_string(), yaml);
        assert_eq!(render_documents(yaml), expected, "{yaml:?}");
    }
    assert!(Document::from_str("").unwrap().is_empty());
}

#[test]
fn document_from_str_ignoring_comments() {
    let yaml = "# leading\n%YAML 1.2\n---\nkey: value\n";
    let doc = Document::from_str_ignoring_comments(yaml).unwrap();
    assert_eq!(doc.get_string("key"), Some("value".to_string()));
    // Stream-level content is dropped, so the document round-trips to
    // just its own body.
    assert_eq!(doc.to_string(), "---\nkey: value\n");

    for yaml in ["---\na: 1\n---\nb: 2\n", "a\n---\nb\n"] {
        assert_eq!(
            format!(
                "{:?}",
                Document::from_str_ignoring_comments(yaml).unwrap_err()
            ),
            format!(
                "{:?}",
                YamlError::InvalidOperation {
                    operation: "Document::from_str_ignoring_comments".to_string(),
                    reason: "Input contains multiple YAML documents. \
                         Use YamlFile::from_str() for multi-document YAML."
                        .to_string(),
                }
            ),
            "{yaml:?}"
        );
    }
    assert_eq!(
        format!(
            "{:?}",
            Document::from_str_ignoring_comments("a: [\n").unwrap_err()
        ),
        format!(
            "{:?}",
            YamlError::Parse {
                message: "Unclosed flow sequence. Expected: ']' to close sequence. Found: end of \
                      input. Context: in flow sequence. Suggestion: Add ']' to close the \
                      array, or check for missing commas between elements"
                    .to_string(),
                line: Some(2),
                column: Some(1),
            }
        )
    );
}

/// Unlike `from_str`, `from_str_relaxed` takes input with errors or several
/// documents, and gives the first document.
#[test]
fn document_from_str_relaxed() {
    for (yaml, text, errors, rendered) in [
        (
            "key: value\n",
            "key: value\n",
            vec![],
            Some("map{key: value}"),
        ),
        (
            "key: [unclosed",
            "key: [unclosed",
            vec![
                "1:15: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of \
                 input. Context: in flow sequence. Suggestion: Add ']' to close the array, or \
                 check for missing commas between elements",
            ],
            Some("map{key: [unclosed]}"),
        ),
        ("a: 1\n---\nb: 2\n", "a: 1\n", vec![], Some("map{a: 1}")),
        // A default document when nothing is parsed.
        ("", "---\n", vec![], None),
    ] {
        let (doc, found) = Document::from_str_relaxed(yaml);
        assert_eq!(found, errors, "{yaml:?}");
        assert_eq!(doc.to_string(), text, "{yaml:?}");
        let root = doc.syntax().children().find_map(YamlNode::from_syntax);
        assert_eq!(root.map(|r| render(&r)).as_deref(), rendered, "{yaml:?}");
    }
}

#[test]
fn document_accessors() {
    let doc = Document::from_str("# c\na: 1  # d\nb: [1, # e\n  2]\n").unwrap();
    assert_eq!(doc.len(), 2);
    assert_eq!(
        doc.comments()
            .map(|c| c.text().to_string())
            .collect::<Vec<_>>(),
        ["# c", "# d", "# e"]
    );
    assert_eq!(Document::from_str("- a\n").unwrap().len(), 0);

    for (yaml, key, string) in [
        ("key: hello", "key", Some("hello")),
        (r#"key: "hello \"world\"""#, "key", Some(r#"hello "world""#)),
        ("key: 'it''s fine'", "key", Some("it's fine")),
        ("other: value", "key", None),
        ("key:\n  - a\n  - b", "key", None),
        ("key:\n  nested: value", "key", None),
        ("a: !!str 1\nb: !t [x]\n", "a", Some("1")),
        ("a: !!str 1\nb: !t [x]\n", "b", None),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        assert_eq!(doc.get_string(key).as_deref(), string, "{yaml:?}");
    }

    // The first item is read from the tree, so a comma inside a quoted
    // item does not split it.
    for (yaml, is_sequence, items) in [
        (
            "tags:\n  - alpha\n  - beta\n  - gamma",
            true,
            Some(&["alpha", "beta", "gamma"][..]),
        ),
        (
            "tags: [alpha, beta, gamma]",
            true,
            Some(&["alpha", "beta", "gamma"]),
        ),
        (
            "tags: [\"hello, world\", beta]",
            true,
            Some(&["hello, world", "beta"]),
        ),
        ("tags: []", true, Some(&[])),
        ("name: test", false, None),
        ("tags: test", false, None),
    ] {
        let doc = Document::from_str(yaml).unwrap();
        assert_eq!(doc.is_sequence("tags"), is_sequence, "{yaml:?}");
        let found: Option<Vec<String>> = doc.get_sequence("tags").map(|s| {
            s.values()
                .map(|v| v.as_scalar().unwrap().as_string())
                .collect()
        });
        assert_eq!(
            found,
            items.map(|i| i.iter().map(|s| s.to_string()).collect()),
            "{yaml:?}"
        );
    }
}

#[test]
fn document_api_usage() {
    let doc = Document::new();
    assert!(!doc.contains_key("Repository"));
    doc.set("Repository", "https://github.com/user/repo.git");
    assert_cst_ok(&doc);
    assert!(doc.contains_key("Repository"));
    assert_eq!(
        doc.get_string("Repository"),
        Some("https://github.com/user/repo.git".to_string())
    );
    assert!(!doc.is_empty());
    let keys: Vec<_> = doc.keys().map(|k| k.to_string()).collect();
    assert_eq!(keys, ["Repository"]);

    assert_eq!(
        doc.remove("Repository").unwrap().to_string(),
        "Repository: https://github.com/user/repo.git\n"
    );
    assert_cst_ok(&doc);
    assert!(!doc.contains_key("Repository"));
    assert!(doc.is_empty());
}

#[test]
fn document_file_io() {
    let path = std::env::temp_dir().join(format!("test_yaml_edit-{}.yaml", std::process::id()));
    let doc = Document::new();
    doc.set("Name", "TestProject");
    assert_cst_ok(&doc);
    doc.set("Repository", "https://example.com/repo.git");
    assert_cst_ok(&doc);
    doc.to_file(&path).unwrap();

    let loaded_doc = Document::from_file(&path).unwrap();
    assert_eq!(
        loaded_doc.to_string(),
        "---\nName: TestProject\nRepository: https://example.com/repo.git\n"
    );
    assert_eq!(
        loaded_doc.get_string("Name"),
        Some("TestProject".to_string())
    );
    assert_eq!(
        loaded_doc.get_string("Repository"),
        Some("https://example.com/repo.git".to_string())
    );
    std::fs::remove_file(&path).unwrap();

    let dir = std::env::temp_dir().join(format!("yaml-edit-doc-{}", std::process::id()));
    let path = dir.join("sub").join("doc.yaml");
    // to_file creates the directory and ends the file with a newline.
    let doc = Document::from_str("a: 1").unwrap();
    doc.to_file(&path).unwrap();
    assert_eq!(std::fs::read_to_string(&path).unwrap(), "a: 1\n");

    std::fs::write(&path, "a: [\n").unwrap();
    let (doc, errors) = Document::from_file_relaxed(&path).unwrap();
    assert_eq!(doc.to_string(), "a: [\n");
    assert_eq!(
        errors,
        [
            "2:1: Unclosed flow sequence. Expected: ']' to close sequence. Found: end of input. \
          Context: in flow sequence. Suggestion: Add ']' to close the array, or check for \
          missing commas between elements"
        ]
    );
    std::fs::remove_dir_all(&dir).unwrap();
    assert_eq!(
        Document::from_file_relaxed(&path).unwrap_err().kind(),
        std::io::ErrorKind::NotFound
    );
}

/// A built document has the same CST as the parser gives its text, and a
/// mapping keeps the lines after its last entry in a document made from
/// it.
#[test]
fn built_documents_are_parser_shaped() {
    let mapping = Mapping::new();
    mapping.set("a", 1);
    assert_cst_ok(&mapping);
    for doc in [Document::new(), Document::from_mapping(mapping)] {
        let text = doc.to_string();
        let parsed = YamlFile::from_str(&text).unwrap().document().unwrap();
        assert_eq!(
            debug::shape_to_string(doc.syntax()),
            debug::shape_to_string(parsed.syntax()),
            "{text:?}"
        );
    }

    for (src, expected) in [
        ("a: 1\n# c\n", "---\na: 1\n# c\n"),
        ("a: 1\n\n", "---\na: 1\n\n"),
        ("a: 1", "---\na: 1"),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        let mapping = file.document().unwrap().as_mapping().unwrap();
        let doc = Document::from_mapping(mapping);
        assert_cst_ok(&doc);
        assert_eq!(doc.to_string(), expected);
        let parsed = YamlFile::from_str(expected).unwrap().document().unwrap();
        assert_eq!(
            debug::shape_to_string(doc.syntax()),
            debug::shape_to_string(parsed.syntax())
        );
    }
}

/// Adding the first key to a document without a root keeps the tokens it
/// has, and puts the new mapping where the parser would read it.
#[test]
fn add_root_to_rootless_document() {
    for (src, expected) in [
        ("---\n", "---\nk: v\n"),
        ("---", "---\nk: v\n"),
        ("--- \n", "--- \nk: v\n"),
        ("--- # c\n", "--- # c\nk: v\n"),
        ("--- # c", "--- # c\nk: v\n"),
        ("---\n# c\n", "---\n# c\nk: v\n"),
        ("%YAML 1.2\n---\n# c\n", "%YAML 1.2\n---\n# c\nk: v\n"),
        ("\u{feff}---\n", "\u{feff}---\nk: v\n"),
        ("---\n...\n", "---\nk: v\n...\n"),
        ("--- # c\n...\n", "--- # c\nk: v\n...\n"),
        ("---\n...", "---\nk: v\n..."),
    ] {
        let insert = |doc: &Document| doc.insert_at_index(0, "k", "v");
        let set = |doc: &Document| doc.set("k", "v");
        let set_ordered = |doc: &Document| doc.set_with_field_order("k", "v", ["k"]);
        for add in [&insert as &dyn Fn(&Document) -> bool, &set, &set_ordered] {
            let file = YamlFile::from_str(src).unwrap();
            let doc = file.document().unwrap();
            assert!(add(&doc), "{src:?}");
            assert_file_edited(&file, expected);
        }
    }
}

/// The directives of a parsed stream, as (name, value, text, is a %YAML
/// directive, is a %TAG directive).
#[test]
fn parsed_directives() {
    type Parts = (Option<String>, Option<String>, String, bool, bool);
    let parts = |yaml: &str| -> Vec<Parts> {
        let file = YamlFile::from_str(yaml).unwrap();
        assert_eq!(file.to_string(), yaml);
        file.directives()
            .map(|d| {
                (
                    d.name(),
                    d.value(),
                    d.text(),
                    d.is_yaml_version(),
                    d.is_tag(),
                )
            })
            .collect()
    };
    let yaml = |value: &str| {
        (
            Some("YAML".to_string()),
            Some(value.to_string()),
            format!("%YAML {value}"),
            true,
            false,
        )
    };
    let tag = (
        Some("TAG".to_string()),
        Some("! tag:example.com,2000:app/".to_string()),
        "%TAG ! tag:example.com,2000:app/".to_string(),
        false,
        true,
    );
    assert_eq!(parts("%YAML 1.2\n---\nkey: value"), [yaml("1.2")]);
    assert_eq!(
        parts("%TAG ! tag:example.com,2000:app/\n---\nkey: value"),
        std::slice::from_ref(&tag)
    );
    assert_eq!(
        parts("%YAML 1.2\n%TAG ! tag:example.com,2000:app/\n---\nkey: value"),
        [yaml("1.2"), tag.clone()]
    );
    assert_eq!(
        parts("%YAML 1.2\n%TAG ! tag:example.com,2000:app/\n---\nkey: value\n"),
        [yaml("1.2"), tag]
    );
    assert_eq!(
        parts("%FOO\n%\n---\na\n"),
        [
            (
                Some("FOO".to_string()),
                None,
                "%FOO".to_string(),
                false,
                false
            ),
            (None, None, "%".to_string(), false, false),
        ]
    );
}

#[test]
fn new_directive() {
    let directive = Directive::new_yaml_version("1.2");
    assert!(directive.is_yaml_version());
    assert_eq!(directive.value(), Some("1.2".to_string()));
    assert_eq!(directive.text(), "%YAML 1.2");

    let directive = Directive::new_tag("!", "tag:example.com,2000:app/");
    assert!(directive.is_tag());
    assert_eq!(directive.text(), "%TAG ! tag:example.com,2000:app/");
}

/// A directive added to a stream goes first, and the document after it
/// gets a `---` if it had none.
#[test]
fn add_directive() {
    let file = YamlFile::new();
    file.add_directive("%YAML 1.2");
    assert_file_edited(&file, "%YAML 1.2\n");
    assert_eq!(file.directives().count(), 1);

    for (src, directive, expected) in [
        ("a: 1\n", "%YAML 1.2", "%YAML 1.2\n---\na: 1\n"),
        ("---\na: 1\n", "%YAML 1.2", "%YAML 1.2\n---\na: 1\n"),
        ("a: 1", "%YAML 1.2", "%YAML 1.2\n---\na: 1"),
        (
            "%YAML 1.2\n---\na: 1\n",
            "%TAG ! tag:example.com,2000:app/",
            "%TAG ! tag:example.com,2000:app/\n%YAML 1.2\n---\na: 1\n",
        ),
    ] {
        let file = YamlFile::from_str(src).unwrap();
        file.add_directive(directive);
        assert_file_edited(&file, expected);
    }

    let reparsed = YamlFile::from_str("%YAML 1.2\n---\na: 1\n").unwrap();
    assert_eq!(reparsed.directives().count(), 1);
    let docs: Vec<_> = reparsed.documents().collect();
    assert_eq!(docs.len(), 1);
    assert_eq!(docs[0].get("a").and_then(|v| v.to_i64()), Some(1));

    let file = YamlFile::new();
    file.add_directive("%YAML 1.2");
    assert_file_cst_ok(&file);
    let doc = Document::new_mapping();
    doc.set("name", "test");
    assert_cst_ok(&doc);
    file.push_document(doc);
    assert_file_edited(&file, "%YAML 1.2\n---\nname: test\n");
    assert_eq!(file.directives().count(), 1);
    assert_eq!(file.documents().count(), 1);
}
