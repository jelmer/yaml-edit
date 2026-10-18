//! Guards against unintended lexer changes: hashes the token stream and
//! whitespace errors for every YAML test suite input and compares them with
//! `tests/lex_snapshot.txt`.
//!
//! After an intended lexer change, regenerate the snapshot with
//! `UPDATE_LEX_SNAPSHOT=1 cargo test --test lex_snapshot`.

use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

const SNAPSHOT: &str = "tests/lex_snapshot.txt";

/// FNV-1a, so the hashes do not depend on the standard library's hasher.
fn fnv1a(data: &[u8]) -> u64 {
    data.iter().fold(0xcbf29ce484222325, |hash, byte| {
        (hash ^ u64::from(*byte)).wrapping_mul(0x100000001b3)
    })
}

fn lex_hash(input: &str) -> u64 {
    let (tokens, errors) = yaml_edit::lex_with_validation(input);
    let mut repr = String::new();
    for (kind, text) in tokens {
        repr.push_str(&format!("{kind:?} {text:?}\n"));
    }
    for error in errors {
        repr.push_str(&format!("{error:?}\n"));
    }
    fnv1a(repr.as_bytes())
}

/// Undo the CRLF conversion a Windows checkout may apply: the files use LF
/// line endings, and none of the test suite inputs contain a carriage return.
fn lf(text: String) -> String {
    text.replace("\r\n", "\n")
}

/// Hash every `in.yaml` below `test-data`, keyed by its case directory.
fn current_hashes() -> BTreeMap<String, u64> {
    let mut hashes = BTreeMap::new();
    let mut dirs = vec![Path::new("test-data").to_path_buf()];
    while let Some(dir) = dirs.pop() {
        for entry in fs::read_dir(&dir).unwrap() {
            let path = entry.unwrap().path();
            let name = path.file_name().unwrap().to_str().unwrap();
            if path.is_dir() {
                // Metadata directories link to the cases themselves.
                if dir != Path::new("test-data") || !matches!(name, "name" | "tags") {
                    dirs.push(path);
                }
            } else if name == "in.yaml" {
                // Join with `/` so the keys match the snapshot on Windows too.
                let case: Vec<_> = dir
                    .strip_prefix("test-data")
                    .unwrap()
                    .iter()
                    .map(|c| c.to_str().unwrap())
                    .collect();
                let input = fs::read_to_string(&path).unwrap();
                hashes.insert(case.join("/"), lex_hash(&lf(input)));
            }
        }
    }
    assert!(
        !hashes.is_empty(),
        "no YAML test suite inputs found; run `git submodule update --init`"
    );
    hashes
}

#[test]
fn lexer_output_matches_snapshot() {
    let current = current_hashes();
    let rendered: String = current
        .iter()
        .map(|(case, hash)| format!("{case} {hash:016x}\n"))
        .collect();
    if std::env::var_os("UPDATE_LEX_SNAPSHOT").is_some() {
        fs::write(SNAPSHOT, rendered).unwrap();
        return;
    }
    let expected = lf(fs::read_to_string(SNAPSHOT).unwrap());
    let changed: Vec<&str> = expected
        .lines()
        .zip(rendered.lines())
        .filter(|(a, b)| a != b)
        .map(|(a, _)| a)
        .collect();
    assert_eq!(
        expected, rendered,
        "lexer output changed for {changed:?}; if intended, rerun with UPDATE_LEX_SNAPSHOT=1"
    );
}
