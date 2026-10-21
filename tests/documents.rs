//! Document markers, directives and streams of several documents.

mod common;
use common::{assert_file_cst_ok, parse_clean, render_documents};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{debug, SyntaxKind, YamlFile, YamlValue};

#[test]
fn document_streams() {
    for (yaml, documents) in [
        ("---\na: 1\n", &["map{a: 1}"][..]),
        ("--- a\n", &["a"]),
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
