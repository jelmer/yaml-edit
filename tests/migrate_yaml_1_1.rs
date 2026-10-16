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
    assert_eq!(ScalarValue::parse("0755").to_i64(), Some(755));
    assert_eq!(ScalarValue::parse("0o755").to_i64(), Some(493));
}

#[test]
fn a_negative_bare_octal_becomes_decimal() {
    // 1.2 has no signed `0o` form: `-0o755` is a string there.
    assert_eq!(migrated("n: -0755\n"), "n: -493\n");
    assert_eq!(migrated("n: +0755\n"), "n: 0o755\n");
}

#[test]
fn an_all_zero_octal_stays_zero() {
    assert_eq!(
        migrated("a: 00\nb: 000\nc: -00\n"),
        "a: 0o0\nb: 0o0\nc: 0\n"
    );
}

#[test]
fn the_single_letter_booleans_are_left_alone() {
    let input = "a: y\nb: Y\nc: n\nd: N\ny: 1\n";
    assert_eq!(migrated(input), input);
}

#[test]
fn underscores_are_dropped_from_numbers() {
    assert_eq!(migrated("a: 1_000\n"), "a: 1000\n");
    assert_eq!(migrated("a: -1_000\n"), "a: -1000\n");
    assert_eq!(migrated("a: 0x_1F\n"), "a: 0x1F\n");
    assert_eq!(migrated("a: 0_755\n"), "a: 0o755\n");
    assert_eq!(migrated("a: 1_000.5_0\n"), "a: 1000.50\n");
    assert_eq!(migrated("a: 1_0.5e+5\n"), "a: 10.5e+5\n");
    assert_eq!(migrated("a: 0_5.5\n"), "a: 05.5\n");
}

#[test]
fn binary_and_signed_hex_become_decimal() {
    assert_eq!(migrated("a: 0b1010\n"), "a: 10\n");
    assert_eq!(migrated("a: -0b1_0\n"), "a: -2\n");
    assert_eq!(migrated("a: -0x1F\n"), "a: -31\n");
    assert_eq!(migrated("a: +0x1F\n"), "a: 31\n");
}

#[test]
fn sexagesimal_numbers_become_decimal() {
    assert_eq!(migrated("t: 1:30\n"), "t: 90\n");
    assert_eq!(migrated("t: -1:30\n"), "t: -90\n");
    assert_eq!(migrated("t: 190:20:30\n"), "t: 685230\n");
    assert_eq!(migrated("t: 1:30.5\n"), "t: 90.5\n");
    assert_eq!(migrated("t: 0:30.\n"), "t: 30.0\n");
    assert_eq!(migrated("t: 190:20:30.15\n"), "t: 685230.15\n");
}

#[test]
fn near_misses_of_the_1_1_forms_are_left_alone() {
    for src in [
        "a: 1:60\n",
        "a: 01:30\n",
        "a: 1:30:\n",
        "a: 0b102\n",
        "a: 0x_\n",
        "a: _1\n",
        "a: 0_8\n",
        "a: 1_000a\n",
        "a: yy\n",
        // 1.1 floats need a signed exponent.
        "a: 1_0.5e5\n",
    ] {
        assert_eq!(migrated(src), src, "{src:?}");
    }
}

#[test]
fn strings_that_1_2_reads_as_numbers_are_quoted() {
    // Neither 1.1 octal (it has an 8 or 9) nor 1.1 decimal (which has no
    // leading zero), so a 1.1 string; a 1.2 integer.
    assert_eq!(migrated("a: 09\n"), "a: '09'\n");
    assert_eq!(migrated("a: -0789\n"), "a: '-0789'\n");
    assert_eq!(migrated("a: +08\n"), "a: '+08'\n");
    // 1.1 had no `0o` octal.
    assert_eq!(migrated("a: 0o17\n"), "a: '0o17'\n");
    // 1.1 floats need a `.` and a signed exponent.
    assert_eq!(migrated("a: 1e3\n"), "a: '1e3'\n");
    assert_eq!(migrated("a: -1E+3\n"), "a: '-1E+3'\n");
    assert_eq!(migrated("a: 1.0e3\n"), "a: '1.0e3'\n");
    assert_eq!(migrated("a: .5E3\n"), "a: '.5E3'\n");
    // Keys too.
    assert_eq!(migrated("09: a\n1e3: b\n"), "'09': a\n'1e3': b\n");
    assert_eq!(
        migrated("f: [09, {1e3: 0o7}]\n"),
        "f: ['09', {'1e3': '0o7'}]\n"
    );
}

#[test]
fn numbers_in_both_versions_are_left_alone() {
    for src in [
        "a: 0\n",
        "a: -12\n",
        "a: 0x1F\n",
        "a: 1.5\n",
        "a: .5\n",
        "a: 1.\n",
        "a: 1.0e+3\n",
        "a: .5E-3\n",
        "a: 1.e-3\n",
        "a: -.inf\n",
        "a: .NaN\n",
        "a: ~\n",
        "a: TRUE\n",
        "a: !!int 0o17\n",
        "a: !!float 1e3\n",
        "a: '09'\n",
        "a: \"1e3\"\n",
    ] {
        assert_eq!(migrated(src), src, "{src:?}");
    }
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
        "fine: 42\n",
        "fine: true\n",
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
    let doc = Document::from_str("mode: 0755\nflag: yes\nkeep: 1\nid: 09\n").unwrap();
    let rewrites = to_yaml_1_2(&doc);
    assert_eq!(rewrites.len(), 3);
    assert_eq!(rewrites[0].before, "0755");
    assert_eq!(rewrites[0].after, "0o755");
    assert_eq!(rewrites[1].before, "yes");
    assert_eq!(rewrites[1].after, "true");
    assert_eq!(rewrites[2].before, "09");
    assert_eq!(rewrites[2].after, "'09'");
}

#[test]
fn a_migrated_document_still_parses_and_is_stable() {
    let src = "flag: yes\nnested:\n  - no\n  - 1_000\n  - 09\n  - 1e3\n";
    let once = migrated(src);
    Document::from_str(&once).unwrap();
    // Migrating again finds nothing left to do.
    assert_eq!(migrated(&once), once);
}

#[test]
fn migrating_twice_quotes_the_octal_it_wrote() {
    // `0o755` is a string to a 1.1 reader, so the output of a migration is
    // not itself a 1.1 document.
    let once = migrated("mode: 0755\n");
    assert_eq!(once, "mode: 0o755\n");
    assert_eq!(migrated(&once), "mode: '0o755'\n");
}
