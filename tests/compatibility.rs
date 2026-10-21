//! Inputs where yaml-edit reads YAML differently from PyYAML or saphyr.
//!
//! Each was checked against the YAML 1.2 spec and both parsers and left as
//! it is; docs/compatibility.md explains why.

mod common;
use common::{parse_clean, render_documents, violations};

fn strings(pairs: &[(&str, &str)]) -> Vec<(String, String)> {
    pairs
        .iter()
        .map(|(a, b)| (a.to_string(), b.to_string()))
        .collect()
}

/// `c-l-block-map-explicit-value(n)` wants the `:` at the mapping's own
/// column, so PyYAML rejects all of these, and saphyr accepts only the
/// first. The parser accepts them alike; the validator reports them.
#[test]
fn explicit_value_indicator_indented_past_mapping() {
    for (yaml, expected, location) in [
        ("?\n  : c\n", "map{~: c}", "2:3"),
        ("? a\n  : c\n", "map{a: c}", "2:3"),
        ("?\n  x\n  : c\n", "map{x: c}", "3:3"),
    ] {
        assert_eq!(render_documents(yaml), [expected], "{yaml:?}");
        assert_eq!(
            violations(&parse_clean(yaml)),
            strings(&[(
                "Explicit value indicator ':' must be at the column of its '?'",
                location
            )]),
            "{yaml:?}"
        );
    }
    let yaml = "?\n: c\n";
    assert_eq!(render_documents(yaml), ["map{~: c}"]);
    assert_eq!(violations(&parse_clean(yaml)), []);
}

/// Read as the tag `!!:` on the scalar `v`, as PyYAML does; saphyr reports
/// the document as bad.
#[test]
fn secondary_tag_handle_with_colon_suffix() {
    let yaml = "!!: v\n";
    let file = parse_clean(yaml);
    let tagged = file.document().unwrap().as_tagged().unwrap();
    assert_eq!(tagged.tag().as_deref(), Some("!!:"));
    assert_eq!(tagged.value().unwrap().as_string(), "v");
    assert_eq!(violations(&file), []);
}

/// Two entries with equal null keys. saphyr's parser emits the same
/// events; only its loader collapses them. The validator reports the
/// duplicate, as for any other key.
#[test]
fn repeated_empty_keys() {
    for yaml in ["- :\n  :\n", "- ?\n  ?\n"] {
        assert_eq!(render_documents(yaml), ["seq[map{~: ~, ~: ~}]"], "{yaml:?}");
        assert_eq!(
            violations(&parse_clean(yaml)),
            strings(&[("Duplicate key: \"\" (semantically equal to \"\")", "2:3")]),
            "{yaml:?}"
        );
    }
}
