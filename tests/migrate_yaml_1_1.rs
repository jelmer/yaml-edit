//! Migrating YAML 1.1 constructs to their 1.2 spellings.

use std::str::FromStr;
use yaml_edit::migrate::to_yaml_1_2;
use yaml_edit::{Document, ScalarValue};

fn migrated(src: &str) -> String {
    let doc = Document::from_str(src).unwrap();
    to_yaml_1_2(&doc);
    doc.to_string()
}

#[test]
fn a_bare_octal_keeps_its_value() {
    // The point of migrating: `0755` meant 493 to a 1.1 reader and means
    // 755 to a 1.2 one, so rewrite it to the spelling that still means 493.
    assert_eq!(migrated("mode: 0755\n"), "mode: 0o755\n");
    assert_eq!(ScalarValue::parse("0755").to_i64(), Some(493));
    assert_eq!(ScalarValue::parse("0o755").to_i64(), Some(493));
}

#[test]
fn a_negative_bare_octal_keeps_its_sign() {
    assert_eq!(migrated("n: -0755\n"), "n: -0o755\n");
    assert_eq!(ScalarValue::parse("-0o755").to_i64(), Some(-493));
}

#[test]
fn the_legacy_booleans_become_real_ones() {
    assert_eq!(migrated("a: yes\nb: no\n"), "a: true\nb: false\n");
    assert_eq!(migrated("a: ON\nb: Off\n"), "a: true\nb: false\n");
}

#[test]
fn an_unambiguous_scalar_is_left_alone() {
    for src in [
        "keep: 'yes'\n",
        "keep: \"0755\"\n",
        "keep: !!str yes\n",
        "fine: 0o644\n",
        "fine: 42\n",
        "fine: true\n",
        // `08` is no octal at all, so there is no 1.1 reading to preserve.
        "bad: 08\n",
        // A block scalar's body is literal text, not a plain scalar.
        "block: |\n  yes\n",
    ] {
        assert_eq!(migrated(src), src, "{src:?}");
    }
}

#[test]
fn migration_reaches_nested_and_flow_values() {
    assert_eq!(
        migrated("a:\n  b:\n    - yes\n    - 0755\n"),
        "a:\n  b:\n    - true\n    - 0o755\n"
    );
    assert_eq!(migrated("f: [yes, 0755]\n"), "f: [true, 0o755]\n");
    assert_eq!(migrated("m: {a: yes}\n"), "m: {a: true}\n");
}

#[test]
fn formatting_and_comments_survive() {
    assert_eq!(
        migrated("mode: 0755 # the mode\nother: 1\n"),
        "mode: 0o755 # the mode\nother: 1\n"
    );
}

#[test]
fn the_rewrites_are_reported() {
    let doc = Document::from_str("mode: 0755\nflag: yes\nkeep: 1\n").unwrap();
    let rewrites = to_yaml_1_2(&doc);
    assert_eq!(rewrites.len(), 2);
    assert_eq!(rewrites[0].before, "0755");
    assert_eq!(rewrites[0].after, "0o755");
    assert_eq!(rewrites[1].before, "yes");
    assert_eq!(rewrites[1].after, "true");
}

#[test]
fn a_migrated_document_still_parses_and_is_stable() {
    let src = "mode: 0755\nflag: yes\nnested:\n  - no\n  - 010\n";
    let once = migrated(src);
    Document::from_str(&once).unwrap();
    // Migrating again finds nothing left to do.
    assert_eq!(migrated(&once), once);
}
