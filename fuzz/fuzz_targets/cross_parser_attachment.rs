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

#[path = "common/saphyr_input.rs"]
mod saphyr_input;

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
    if saphyr_input::unsuitable(input) {
        return;
    }

    // Only inputs a conformant parser accepts can hold yaml-edit to this
    // standard. saphyr rejecting the input means an ERROR node is fair.
    if Yaml::load_from_str(input).is_err() {
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
