#![no_main]

//! A document must mean the same whatever its line breaks, and with or
//! without a byte order mark.
//!
//! YAML treats CR LF, CR and LF alike as line breaks, normalising them to LF
//! in scalar content, and allows a BOM at the start of the stream. For
//! input written with LF line breaks, the variants with CR LF, with CR and
//! with a leading BOM must parse with the same errors, round trip, pass the
//! same validator checks and read as the same values.

use libfuzzer_sys::fuzz_target;
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::validator::{Severity, Validator};
use yaml_edit::{debug, Parse, YamlFile, YamlValue};

#[derive(Debug, PartialEq)]
struct Reading {
    parse_errors: usize,
    validator_errors: usize,
    values: Option<Vec<Option<YamlValue>>>,
}

fn read(input: &str) -> Reading {
    let parse: Parse<YamlFile> = Parse::parse_yaml(input);
    let tree = parse.tree();
    if tree.to_string() != input {
        panic!("round trip changed {input:?} to {:?}", tree.to_string());
    }
    let validator_errors = Validator::new()
        .validate_syntax(tree.syntax())
        .into_iter()
        .filter(|v| v.severity == Severity::Error)
        .count();
    // Only a tree of valid input has to meet the CST invariants.
    if parse.errors().is_empty() && validator_errors == 0 {
        if let Err(e) = debug::validate_tree(tree.syntax()) {
            panic!("CST broken: {e}\ninput: {input:?}");
        }
    }
    let values = YamlFile::from_str(input).ok().map(|file| {
        file.documents()
            // Error messages quote the input, line breaks included.
            .map(|doc| YamlValue::from_document(&doc).ok())
            .collect()
    });
    Reading {
        parse_errors: parse.errors().len(),
        validator_errors,
        values,
    }
}

fuzz_target!(|data: &[u8]| {
    let Ok(input) = std::str::from_utf8(data) else {
        return;
    };
    if input.len() > 10_000 || input.contains(['\r', '\u{feff}']) {
        return;
    }
    let lf = read(input);
    let variants = [
        ("CR LF", input.replace('\n', "\r\n")),
        ("CR", input.replace('\n', "\r")),
        ("BOM", format!("\u{feff}{input}")),
    ];
    for (name, variant) in variants {
        let other = read(&variant);
        if other != lf {
            panic!("{name} reads differently\ninput: {input:?}\nLF:  {lf:?}\n{name}: {other:?}");
        }
    }
});
