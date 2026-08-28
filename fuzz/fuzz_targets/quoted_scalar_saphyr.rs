#![no_main]

//! Cross-check yaml-edit's quoted-scalar decoder against saphyr.
//!
//! Quoted YAML scalars (`"..."` and `'...'`) have a spec-strict
//! decoding: escape sequences, `''`-doubling in single quotes, and
//! line folding. Both parsers must produce the same decoded string
//! byte-for-byte -- there is no leniency zone.
//!
//! Strategy: parse with yaml-edit, walk every scalar whose CST text
//! starts with `"` or `'`, and re-parse that same quoted literal via
//! saphyr's scalar parser. Compare the decoded strings.
//!
//! Plain and block scalars are already covered (plain in
//! `cross_parser_saphyr` for tag resolution; block scalars have
//! their own decoding subtleties worth a separate target later).

use libfuzzer_sys::fuzz_target;
use saphyr::{LoadableYamlNode, ScalarOwned, YamlOwned};
use std::str::FromStr;
use yaml_edit::{Document, YamlNode};

/// The escape starters listed in YAML 1.2.2 Table 5.7, plus `\<line-break>`
/// (line continuation, §7.3.2) and `\<tab>`. Anything else after a `\` is
/// an invalid escape and the two parsers may legitimately disagree about
/// recovery.
fn contains_invalid_double_quoted_escape(inner: &str) -> bool {
    let mut chars = inner.chars();
    while let Some(c) = chars.next() {
        if c != '\\' {
            continue;
        }
        let Some(next) = chars.next() else {
            return true; // trailing backslash
        };
        if !matches!(
            next,
            '0' | 'a'
                | 'b'
                | 't'
                | 'n'
                | 'v'
                | 'f'
                | 'r'
                | 'e'
                | ' '
                | '"'
                | '/'
                | '\\'
                | 'N'
                | '_'
                | 'L'
                | 'P'
                | 'x'
                | 'u'
                | 'U'
                | '\n'
                | '\t'
        ) {
            return true;
        }
    }
    false
}

fn walk(node: YamlNode, input: &str) {
    match node {
        YamlNode::Scalar(scalar) => {
            let raw = scalar.value();
            if !(raw.starts_with('"') && raw.ends_with('"'))
                && !(raw.starts_with('\'') && raw.ends_with('\''))
            {
                return;
            }
            // Skip double-quoted content that carries invalid escapes
            // (`\` followed by anything outside YAML 1.2 Table 5.7).
            // Per the spec this is an error, and yaml-edit and saphyr
            // pick different lenient-recovery strategies (yaml-edit
            // preserves the backslash; saphyr drops it) -- comparing
            // recovery isn't the oracle's job.
            if raw.starts_with('"') && contains_invalid_double_quoted_escape(&raw[1..raw.len() - 1])
            {
                return;
            }

            // Send the full quoted literal (including quotes) through
            // saphyr's top-level parser -- that's the only entry point
            // that runs escape / fold processing. The scalar-level
            // parse_from_cow_and_metadata treats its input as
            // already-decoded content.
            let Ok(docs) = YamlOwned::load_from_str(&raw) else {
                return;
            };
            let Some(YamlOwned::Value(ScalarOwned::String(theirs))) = docs.into_iter().next()
            else {
                return;
            };
            let mine = scalar.as_string();

            if mine != theirs {
                panic!(
                    "quoted-scalar decoder divergence\n  raw:       {:?}\n  yaml-edit: {:?}\n  saphyr:    {:?}\n  input:\n{}",
                    raw, mine, theirs, input,
                );
            }
        }
        YamlNode::Mapping(m) => {
            for (k, v) in m.iter() {
                walk(k, input);
                walk(v, input);
            }
        }
        YamlNode::Sequence(seq) => {
            for item in seq.values() {
                walk(item, input);
            }
        }
        YamlNode::Alias(_) | YamlNode::TaggedNode(_) => {}
    }
}

fuzz_target!(|data: &[u8]| {
    let Ok(input) = std::str::from_utf8(data) else {
        return;
    };
    if input.len() > 100_000 {
        return;
    }
    if input.bytes().any(|b| b < 0x20 && b != b'\t' && b != b'\n') {
        return;
    }

    let Ok(doc) = Document::from_str(input) else {
        return;
    };
    if let Some(m) = doc.as_mapping() {
        walk(YamlNode::Mapping(m), input);
    } else if let Some(s) = doc.as_sequence() {
        walk(YamlNode::Sequence(s), input);
    } else if let Some(sc) = doc.as_scalar() {
        walk(YamlNode::Scalar(sc), input);
    }
});
