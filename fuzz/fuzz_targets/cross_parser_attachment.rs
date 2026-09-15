#![no_main]

//! Cross-check against saphyr that yaml-edit never silently strands input.
//!
//! The parser is deliberately lenient: tokens it cannot place are swept
//! into an ERROR node and `from_str` still succeeds (see the "consume any
//! remaining tokens as ERROR nodes" loop in `parser/mod.rs`). That is the
//! right behaviour for genuinely invalid input, which also produces a
//! parse error. It is a bug when it happens with *no* reported error on
//! input a conformant parser accepts: the stranded tokens are usually a
//! value plus every sibling entry after it, so `as_mapping()` quietly
//! loses keys while the round trip still looks perfect.
//!
//! That is the shape behind a run of fixes around tags and anchors on
//! block nodes. `cross_parser_saphyr` cannot see it: it walks the CST
//! checking scalar tag resolution, and an orphaned ERROR node is not
//! reachable from the tree.
//!
//! Oracle: if saphyr parses the input and yaml-edit reports no error,
//! then yaml-edit's tree must contain no ERROR node.

use libfuzzer_sys::fuzz_target;
use rowan::ast::AstNode;
use saphyr::{LoadableYamlNode, Yaml};
use yaml_edit::{Parse, SyntaxKind, YamlFile};


/// Whether any line could be a plain scalar continued on a later, more
/// indented line. Multi-line plain scalars are valid YAML that yaml-edit
/// does not implement: the continuation lands in an ERROR node, so it is
/// not the attachment bug this target hunts.
///
/// Deliberately broad. Rather than decide which lines really end in a
/// plain scalar (the thing the parser itself gets wrong), treat any
/// content line that is not unambiguously a block-structure opener as a
/// possible plain scalar. That costs coverage but keeps a known gap from
/// masking the bug class this target is for.
fn has_plain_scalar_continuation(input: &str) -> bool {
    let mut prev_plain_indent: Option<usize> = None;
    for line in input.lines() {
        let trimmed = line.trim();
        if trimmed.is_empty() || trimmed.starts_with('#') {
            continue;
        }
        let indent = line.len() - line.trim_start().len();
        // A plain scalar folds in following lines indented at least as far
        // as itself, so same-indent continuation counts too.
        if prev_plain_indent.is_some_and(|plain| indent >= plain) {
            return true;
        }

        // The value this line leaves open, if any: text after `key: `, or
        // the whole line when it is not a mapping entry.
        let value = match trimmed.split_once(": ") {
            Some((_, v)) => v.trim(),
            None if trimmed.ends_with(':') => "",
            None => trimmed,
        };

        // A value that opens a block collection or a nested node cannot be
        // continued as a plain scalar; anything else might be one. A `-` or
        // `?` counts as an opener only when a space follows it.
        let opens_block = value.is_empty()
            || value.starts_with(['[', '{', '|', '>', '#', '*'])
            || matches!(value, "-" | "?")
            || value.starts_with("- ")
            || value.starts_with("? ");
        // A value made only of node properties (`!!seq`, `&a`, `!!seq &a`)
        // annotates a block node on a later line rather than being a plain
        // scalar, so nothing continues from it.
        let annotation_only = !value.is_empty()
            && value
                .split_whitespace()
                .all(|word| word.starts_with(['!', '&']));

        prev_plain_indent = if opens_block || annotation_only {
            None
        } else {
            Some(indent)
        };
    }
    false
}

fn first_error_node(node: &rowan::SyntaxNode<yaml_edit::Lang>) -> Option<String> {
    if node.kind() == SyntaxKind::ERROR {
        return Some(node.text().to_string());
    }
    node.children().find_map(|child| first_error_node(&child))
}

fuzz_target!(|data: &[u8]| {
    let Ok(input) = std::str::from_utf8(data) else {
        return;
    };
    if input.len() > 100_000 {
        return;
    }
    // C0 controls other than tab and newline are not valid YAML 1.2 and
    // parsers handle them inconsistently.
    if input.bytes().any(|b| b < 0x20 && b != b'\t' && b != b'\n') {
        return;
    }

    // A bare `:` opening a block mapping is a spec grey area: saphyr reads
    // it as a null-keyed entry, PyYAML rejects it outright. Neither answer
    // is authoritative, so skip rather than police a disagreement between
    // reference parsers.
    if input
        .lines()
        .any(|line| line.trim_start().starts_with(':'))
    {
        return;
    }

    // A `:` straight after an anchor or alias name is another grey area:
    // saphyr reads `k: &a: v` as `k: v` and drops the anchor, PyYAML
    // rejects it as "mapping values are not allowed here". With the
    // reference parsers split there is no answer to hold yaml-edit to.
    if input.lines().any(|line| {
        line.split_whitespace()
            .any(|word| word.starts_with(['&', '*']) && word.contains(':'))
    }) {
        return;
    }

    // Only inputs a conformant parser accepts can hold yaml-edit to this
    // standard. saphyr rejecting the input means an ERROR node is fair.
    if Yaml::load_from_str(input).is_err() {
        return;
    }

    // Multi-line plain scalars (a plain scalar continued on following, more
    // indented lines) are not implemented: the continuation always lands in
    // an ERROR node, with or without a blank line, so it is not the
    // attachment bug this target hunts. Skip inputs whose first content
    // line is a plain scalar that something indented follows, so that known
    // gap does not mask everything else.
    if has_plain_scalar_continuation(input) {
        return;
    }

    let parse: Parse<YamlFile> = Parse::parse_yaml(input);
    // A reported parse error is yaml-edit saying so out loud; the ERROR
    // node is then the documented representation, not a silent drop.
    if !parse.errors().is_empty() {
        return;
    }

    let file = parse.tree();
    if let Some(stranded) = first_error_node(file.syntax()) {
        panic!(
            "stranded tokens with no reported parse error\n\
             saphyr parsed this input, so the ERROR node is a silent drop.\n\
             stranded text: {stranded:?}\n\
             input:\n{input}"
        );
    }
});

