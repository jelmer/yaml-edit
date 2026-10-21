//! Checks on the layout of block collections: tabs, indentation and placement.

use super::{Rule, Violation};
use crate::column::ColumnIndex;
use crate::nodes::{
    child_token, has_child_token, is_block_scalar, is_quoted_scalar, starts_a_flow_collection,
};
use crate::yaml::SyntaxNode;

/// Check for a tab used as indentation among `node`'s own tokens.
pub(super) fn check_tab_usage(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::SyntaxKind;
    let tab = match node.kind() {
        SyntaxKind::SCALAR => {
            block_scalar_tab_indent(node).or_else(|| quoted_scalar_tab_indent(node))
        }
        // Only an INDENT that supplies a node's indentation can hold
        // an illegal tab. YAML 1.2 forbids a tab in `s-indent`, but
        // elsewhere -- inside a scalar's continuation line, or as
        // separation at the stream's own level -- it is content or
        // legal whitespace, which is how the test suite reads 4ZYM,
        // HS5T, NB6Z, UV7Q, 6CA3, K54U and T5N4.
        SyntaxKind::DOCUMENT
        | SyntaxKind::KEY
        | SyntaxKind::VALUE
        | SyntaxKind::MAPPING
        | SyntaxKind::SEQUENCE
        | SyntaxKind::MAPPING_ENTRY
        | SyntaxKind::SEQUENCE_ENTRY => block_tab_indent(node),
        _ => None,
    };
    if let Some(token) = tab {
        violations.push(Violation::error_at(
            Rule::InvalidTabUsage,
            token.text_range(),
            "Tabs are not allowed for indentation in YAML",
        ));
    }
}

/// Check for multiple mapping entries on the same line in block mappings
///
/// In block mappings (not flow mappings with {}), each mapping entry should
/// be on its own line. Multiple entries on the same line are invalid.
pub(super) fn check_block_mapping_entries_on_same_line(
    node: &SyntaxNode,
    violations: &mut Vec<Violation>,
) {
    // Check if this is a flow mapping (which allows same-line entries)
    let first_token = node.first_token();
    let is_flow_mapping = first_token.as_ref().is_some_and(|t| t.text() == "{");

    if is_flow_mapping {
        return; // Flow mappings can have entries on same line
    }

    let entries: Vec<SyntaxNode> = node
        .children()
        .filter(|c| c.kind() == crate::SyntaxKind::MAPPING_ENTRY)
        .collect();
    // One violation is enough.
    if let Some(pair) = entries.windows(2).find(|w| !newline_between(&w[0], &w[1])) {
        violations.push(Violation::error_at(
            Rule::Other,
            pair[1].text_range(),
            "Block mapping entries must be on separate lines",
        ));
    }
}

/// Whether a line break separates mapping entry `prev` from the entry
/// `next` that follows it.
///
/// The newline may be inside `prev` (as its last token) or between the
/// entries as a sibling token.
fn newline_between(prev: &SyntaxNode, next: &SyntaxNode) -> bool {
    // An explicit key's entry ends with the zero-width implicit-null
    // scalar of its VALUE, so look past tokens that render as nothing:
    // `? a\n? b\n` has its newline inside the first entry, not between
    // them. In a nested mapping the next line's indentation follows that
    // newline (`k:\n  ? a\n  ? b\n`).
    let prev_ends_with_newline = prev
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| {
            !t.text().is_empty()
                && !matches!(
                    t.kind(),
                    crate::SyntaxKind::INDENT | crate::SyntaxKind::WHITESPACE
                )
        })
        .last()
        .is_some_and(|t| t.kind() == crate::SyntaxKind::NEWLINE);
    prev_ends_with_newline
        || prev
            .siblings_with_tokens(rowan::Direction::Next)
            .skip(1)
            .take_while(|el| el.as_node() != Some(next))
            .any(|el| el.kind() == crate::SyntaxKind::NEWLINE)
}

/// Check that sibling MAPPING_ENTRY nodes inside a block mapping
/// share a consistent indent.
///
/// The parser is deliberately lenient: it accepts
///     k1: v1
///      k2: v2
/// and produces two MAPPING_ENTRY children separated by an INDENT
/// " " token as a direct child of MAPPING. Similarly for
///     key:
///       ok: 1
///      wrong: 2
/// (yaml-test-suite EW3V / DMG6 / N4JP / U44R). Flag inconsistent
/// leading indent tokens between block entries.
pub(super) fn check_mapping_entry_indentation(
    node: &SyntaxNode,
    columns: &ColumnIndex,
    violations: &mut Vec<Violation>,
) {
    // Flow mappings ({...}) don't have INDENT tokens between entries.
    if starts_a_flow_collection(node) {
        return;
    }

    // Every INDENT token that precedes a MAPPING_ENTRY (i.e. separates
    // two entries) must equal the expected indent. Otherwise the parser
    // has admitted a wrong-indented sibling that the YAML spec rejects
    // (see yaml-test-suite EW3V / DMG6 / N4JP / U44R).
    let expected_indent = expected_entry_indent(node, columns);

    // Only an INDENT that introduces a sibling entry states that
    // entry's indentation. One followed by a NEWLINE pads a blank
    // line (H2RW), and a trailing one belongs to the enclosing
    // construct rather than to this mapping (V9D5's explicit key).
    let mut seen_entry = false;
    let mut pending: Option<rowan::SyntaxToken<crate::Lang>> = None;
    for child in node.children_with_tokens() {
        match child {
            rowan::NodeOrToken::Token(t) if t.kind() == crate::SyntaxKind::INDENT => {
                pending = Some(t);
            }
            rowan::NodeOrToken::Node(n) if n.kind() == crate::SyntaxKind::MAPPING_ENTRY => {
                if let Some(t) = pending.take() {
                    if seen_entry && t.text() != expected_indent {
                        violations.push(Violation::error_at(
                            Rule::Other,
                            t.text_range(),
                            format!(
                                "Sibling block mapping entries have inconsistent indentation (expected {expected_indent:?}, found {:?})",
                                t.text()
                            ),
                        ));
                    }
                }
                seen_entry = true;
            }
            _ => {
                pending = None;
            }
        }
    }
}

/// The indentation every entry of block mapping `node` should have.
///
/// For a nested mapping (its parent VALUE has NEWLINE + INDENT before it)
/// that is the INDENT's text; otherwise (root, compact or explicit-entry
/// mapping) the column of its first entry.
fn expected_entry_indent(node: &SyntaxNode, columns: &ColumnIndex) -> String {
    // A mapping that is a sequence entry's value has its INDENT in the
    // SEQUENCE_ENTRY rather than a VALUE, in the same position relative
    // to the MAPPING. Without it the expected indent fell back to the
    // empty string and every entry of `-\n  a: 1\n  b: 2\n` looked
    // wrongly indented. A tagged mapping (`k: !t\n  a: 1\n`, C4HZ) has
    // it in the TAGGED_NODE.
    node.parent()
        .filter(|p| {
            matches!(
                p.kind(),
                crate::SyntaxKind::VALUE
                    | crate::SyntaxKind::SEQUENCE_ENTRY
                    | crate::SyntaxKind::TAGGED_NODE
            )
        })
        .and_then(|parent_value| {
            // Look for the INDENT token that immediately precedes
            // this MAPPING in the parent VALUE.
            let mut last_indent: Option<String> = None;
            for el in parent_value.children_with_tokens() {
                match el {
                    rowan::NodeOrToken::Node(n) if &n == node => break,
                    rowan::NodeOrToken::Token(t) if t.kind() == crate::SyntaxKind::INDENT => {
                        last_indent = Some(t.text().to_string());
                    }
                    _ => {}
                }
            }
            last_indent
        })
        .or_else(|| {
            // A compact mapping on a sequence entry's own line has no
            // INDENT of its own (`- key: value\n  key2: value2\n`); its
            // entries line up under the dash's gap. The same goes for one
            // after an explicit `?` or `:`. A root mapping may be
            // indented too (`  a: 1\n  b: 2\n`), and after a leading
            // comment its INDENT is not even in the DOCUMENT. Take the
            // column from the node's offset within its line.
            let parent = node.parent()?;
            if !matches!(
                parent.kind(),
                crate::SyntaxKind::SEQUENCE_ENTRY
                    | crate::SyntaxKind::KEY
                    | crate::SyntaxKind::VALUE
                    | crate::SyntaxKind::DOCUMENT
            ) {
                return None;
            }
            Some(" ".repeat(columns.column(&node.first_token()?)))
        })
        .unwrap_or_default()
}

/// Check that comment tokens have whitespace separation
///
/// YAML spec requires that comment markers (#) must be separated from other content
/// by whitespace. This checks if a COMMENT token appears without preceding whitespace.
pub(super) fn check_comment_token_whitespace(
    token: &rowan::SyntaxToken<crate::Lang>,
    violations: &mut Vec<Violation>,
) {
    // What matters is the token before the comment in document order,
    // not the previous sibling: a comment opening a VALUE (`hr: # c`)
    // has no sibling before it, and the separating whitespace sits one
    // level up. Walking siblings alone reported 5 files the test suite
    // marks valid.
    // An empty node (`? |\n# c`) has a zero-width token, which separates
    // nothing either way.
    let Some(prev) = std::iter::successors(token.prev_token(), |t| t.prev_token())
        .find(|t| !t.text().is_empty())
    else {
        return;
    };
    // A BOM leaves the comment at the start of its line, which is
    // separation enough (l-document-prefix).
    if !matches!(
        prev.kind(),
        crate::SyntaxKind::WHITESPACE
            | crate::SyntaxKind::NEWLINE
            | crate::SyntaxKind::INDENT
            | crate::SyntaxKind::BOM
    ) {
        violations.push(Violation::error_at(
            Rule::Other,
            token.text_range(),
            "Comment without whitespace separation",
        ));
    }
}

/// Check that a block mapping does not start on the line of its tag or
/// anchor, as in `!x ? a`.
///
/// Node properties of a block collection have to be followed by a line
/// break (`s-l+block-collection`). An implicit key on that line takes the
/// properties itself, so only an explicit key gets here.
pub(super) fn check_block_mapping_after_properties(
    node: &SyntaxNode,
    violations: &mut Vec<Violation>,
) {
    if starts_a_flow_collection(node) {
        return;
    }
    let mut prev = node.prev_sibling_or_token();
    while let Some(rowan::NodeOrToken::Token(token)) = &prev {
        if token.kind() != crate::SyntaxKind::WHITESPACE {
            break;
        }
        prev = token.prev_sibling_or_token();
    }
    let Some(rowan::NodeOrToken::Token(property)) = prev else {
        return;
    };
    if matches!(
        property.kind(),
        crate::SyntaxKind::TAG | crate::SyntaxKind::ANCHOR
    ) {
        violations.push(Violation::error_at(
            Rule::Other,
            node.text_range(),
            "A block mapping cannot start on the line of its tag or anchor",
        ));
    }
}

/// Check that implicit keys don't span multiple lines
///
/// YAML spec restricts implicit keys (keys without explicit ? marker) to a single line.
/// This checks if a KEY node in a MAPPING_ENTRY contains newline characters.
/// Two variants of "multiline" show up:
///   1. A NEWLINE token as a direct descendant (unquoted key spanning
///      multiple lexed tokens, e.g. yaml-test-suite 8KB6 in flow context).
///   2. A quoted STRING token whose source text contains a raw '\n'
///      (yaml-test-suite 7LBH / D49Q / JKF3). The whole quoted body
///      is one lex token, so a NEWLINE-token check misses it.
pub(super) fn check_implicit_key_multiline(
    entry_node: &SyntaxNode,
    columns: &ColumnIndex,
    violations: &mut Vec<Violation>,
) {
    // Explicit-key entries (`? key\n : value`) are allowed to span
    // multiple lines by construction; the QUESTION indicator makes
    // them explicit rather than implicit.
    // The `?` sits in the MAPPING_ENTRY for a block explicit key, but
    // inside the KEY for a flow one (`{\n? explicit: entry,\n?\n}`), so
    // look in both -- the test suite's DFF7 has the latter.
    let is_explicit = has_child_token(entry_node, |k| k == crate::SyntaxKind::QUESTION)
        || entry_node
            .children()
            .filter(|c| c.kind() == crate::SyntaxKind::KEY)
            .any(|key| has_child_token(&key, |k| k == crate::SyntaxKind::QUESTION));
    if is_explicit {
        return;
    }

    // A flow scalar may span lines, so a key inside a flow
    // collection is allowed to: the test suite's 8KB6, 9BXH, 9SA2
    // and NJ66 all key an entry with `{ multi\n  line: value}`. The
    // restriction is on an implicit key in block context (7LBH).
    // What matters is the collection this entry belongs to: a flow
    // one lets its keys span lines (8KB6's `- { multi\n  line: v}`),
    // a block one does not, even when the key is itself a flow
    // collection (C2SP's `[23\n]: 42`, DK4H).
    // A flow collection holds its own `{`/`[` as a direct child;
    // first_token() would instead reach into the key's flow
    // sequence, making C2SP's block mapping look like a flow one.
    let in_flow = entry_node
        .parent()
        .is_some_and(|m| starts_a_flow_collection(&m));
    if !in_flow {
        let multiline_key = entry_node
            .children()
            .filter(|c| c.kind() == crate::SyntaxKind::KEY)
            .find(|key| columns.has_line_break(key.text_range()));
        if let Some(key) = multiline_key {
            // Only report once per entry
            violations.push(Violation::error_at(
                Rule::Other,
                key.text_range(),
                "Implicit key cannot span multiple lines",
            ));
            return;
        }
    }

    // The key can also be separated from its own COLON by a line break
    // (`[ "key"\n  :value ]`): the NEWLINE is then a sibling of KEY
    // rather than part of it.
    //
    // Inside a flow *mapping* that is an ordinary entry spread over
    // lines, which the test suite's 5MUD and K3WX (`{ "foo"\n  :bar }`)
    // allow; only a flow sequence needs an implicit key there, which is
    // ZXT5's error.
    if entry_node
        .parent()
        .is_some_and(|m| m.kind() == crate::SyntaxKind::MAPPING && starts_a_flow_collection(&m))
    {
        return;
    }
    if line_break_before_colon(entry_node) {
        violations.push(Violation::error_at(
            Rule::Other,
            entry_node.text_range(),
            "Implicit key cannot span multiple lines",
        ));
    }
}

/// Whether a line break separates the key of mapping entry `entry` from
/// its `:`.
fn line_break_before_colon(entry: &SyntaxNode) -> bool {
    entry
        .children_with_tokens()
        .skip_while(|el| el.kind() != crate::SyntaxKind::KEY)
        .filter_map(|el| el.into_token())
        .find(|t| {
            matches!(
                t.kind(),
                crate::SyntaxKind::NEWLINE | crate::SyntaxKind::COLON
            )
        })
        .is_some_and(|t| t.kind() == crate::SyntaxKind::NEWLINE)
}

/// Check for block sequence or mapping starting on same line as mapping key
///
/// YAML 1.2 spec section 6.3.1 requires block collections to start on a new
/// line after the mapping key and colon. Example of invalid YAML:
/// ```yaml
/// key: - a
///      - b
/// ```
pub(super) fn check_collection_on_same_line_as_key(
    entry_node: &SyntaxNode,
    in_flow: bool,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;

    let key_node = entry_node
        .children()
        .filter(|c| c.kind() == SyntaxKind::KEY)
        .last();
    let value_node = entry_node
        .children()
        .filter(|c| c.kind() == SyntaxKind::VALUE)
        .last();

    // If there's no value, nothing to check
    let Some(value) = value_node else { return };

    // An explicit key's value may be a compact collection on the `:` line
    // (`? k\n: - one\n  - two\n`), which YAML 1.2 allows and PyYAML
    // accepts. Only a plain key's `:` requires the line break, so this
    // rule does not apply to an entry introduced by `?`.
    if has_child_token(entry_node, |k| k == SyntaxKind::QUESTION) {
        return;
    }

    // Only a block collection has to start on a new line, and only in
    // block context: `[k: ? a]` holds a mapping without braces.
    let Some(collection) = value
        .children()
        .find(|c| matches!(c.kind(), SyntaxKind::SEQUENCE | SyntaxKind::MAPPING))
    else {
        return;
    };
    if starts_a_flow_collection(&collection) || in_flow {
        return;
    }

    if !line_break_before_collection(key_node.as_ref(), &value, &collection) {
        let what = if collection.kind() == SyntaxKind::MAPPING {
            "mapping"
        } else {
            "sequence"
        };
        violations.push(Violation::error_at(
            Rule::Other,
            collection.text_range(),
            format!("Block {what} cannot start on same line as mapping key"),
        ));
    }
}

/// Whether a line break separates the `:` after `key` from `collection`,
/// the block collection held in the entry's `value`.
fn line_break_before_collection(
    key: Option<&SyntaxNode>,
    value: &SyntaxNode,
    collection: &SyntaxNode,
) -> bool {
    use crate::SyntaxKind;
    // The parser puts the line break inside the VALUE when the collection
    // is indentless (`k:\n- a\n`), where the entries sit at the key's
    // own column. That is valid YAML, so look there first; the scan
    // below only sees tokens at the MAPPING_ENTRY level.
    let in_value = value
        .children_with_tokens()
        .take_while(|el| el.as_node() != Some(collection))
        .any(|el| el.kind() == SyntaxKind::NEWLINE);
    in_value
        || key.is_some_and(|key| {
            key.siblings_with_tokens(rowan::Direction::Next)
                .filter_map(|el| el.into_token())
                .skip_while(|t| t.kind() != SyntaxKind::COLON)
                .any(|t| t.kind() == SyntaxKind::NEWLINE)
        })
}

/// Check sequence items have consistent indentation (ZVH3)
pub(super) fn check_sequence_indentation(
    node: &SyntaxNode,
    columns: &ColumnIndex,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;

    // Only check SEQUENCE nodes
    if node.kind() != SyntaxKind::SEQUENCE {
        return;
    }

    // Recursively collect all DASH tokens in this sequence and nested sequences
    fn collect_dashes(
        node: &rowan::SyntaxNode<crate::Lang>,
        dashes: &mut Vec<rowan::SyntaxToken<crate::Lang>>,
    ) {
        for child in node.children_with_tokens() {
            match child {
                rowan::NodeOrToken::Token(token) if token.kind() == crate::SyntaxKind::DASH => {
                    dashes.push(token);
                }
                rowan::NodeOrToken::Node(n) if n.kind() == crate::SyntaxKind::SEQUENCE_ENTRY => {
                    // Collect dashes from sequence entries
                    collect_dashes(&n, dashes);
                }
                _ => {}
            }
        }
    }

    let mut dashes = Vec::new();
    collect_dashes(node, &mut dashes);

    // Check if all dashes are at the same column (consistent indentation)
    let Some((first, rest)) = dashes.split_first() else {
        return;
    };
    let first_col = columns.column(first);
    if let Some(misplaced) = rest.iter().find(|t| columns.column(t) != first_col) {
        violations.push(Violation::error_at(
            Rule::InvalidIndentation,
            misplaced.text_range(),
            "Inconsistent sequence item indentation",
        ));
    }
}

/// The tab indenting the body of `node`, a block scalar, if its body holds
/// no content to continue.
///
/// Then a tab line is the body's indentation, which may not be one: the
/// test suite's Y79Y/000 is the error `foo: |\n\t\nbar: 1`.
fn block_scalar_tab_indent(node: &SyntaxNode) -> Option<crate::nodes::SyntaxToken> {
    if !is_block_scalar(node) {
        return None;
    }
    // The body's first indentation, right after the header's line break.
    // The tab has to *be* the indentation. After a space it is content,
    // which is how the suite reads R4YG's `- >\n \t\n detected`.
    child_token(node, crate::SyntaxKind::INDENT).filter(|t| t.text().starts_with('\t'))
}

/// The token of `node`, a quoted scalar, whose continuation line opens with
/// a tab where indentation is required.
///
/// A quoted scalar keeps its continuation lines inside one token, so its
/// text is checked directly: a continuation opening with a tab supplies no
/// indentation at all, which DK95/01's `foo: "bar\n\tbaz"` needs. A blank
/// tab line (DK95/04) indents nothing and is fine. A quoted scalar that is
/// the document's own node needs no indentation, so a tab there is content
/// (7A4E, PRH3), while one inside a mapping value must clear the key's
/// column (DK95/01).
fn quoted_scalar_tab_indent(node: &SyntaxNode) -> Option<crate::nodes::SyntaxToken> {
    if !node.parent().is_some_and(|p| {
        matches!(
            p.kind(),
            crate::SyntaxKind::VALUE | crate::SyntaxKind::SEQUENCE_ENTRY
        )
    }) {
        return None;
    }
    node.children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| {
            is_quoted_scalar(t)
                && t.text()
                    .split(['\n', '\r'])
                    .skip(1)
                    .any(|line| line.starts_with('\t') && !line.trim().is_empty())
        })
}

/// The first token of `node`, a block collection, its entry or a value,
/// that uses a tab as indentation.
fn block_tab_indent(node: &SyntaxNode) -> Option<crate::nodes::SyntaxToken> {
    let is_flow = starts_a_flow_collection(node);
    let tab = node
        .children_with_tokens()
        .filter_map(|c| c.into_token())
        .find(|t| tab_before_block_collection(t) || tab_indent(t, is_flow))?;
    // A flow collection at the stream's own level is not
    // indentation-sensitive, so a tab anywhere inside one is separation
    // whitespace (6CA3's `\t[\n\t]`). Nested in a block collection it
    // still has to clear that block's column, as Y79Y/003's `- [\n\tfoo,`
    // does not. Block collections cannot sit inside flow ones, so the
    // outermost collection tells the two apart. Only a candidate tab pays
    // for the walk up, so deep nesting without tabs stays linear.
    let outermost = node
        .ancestors()
        .filter(|a| {
            matches!(
                a.kind(),
                crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE
            )
        })
        .last();
    if outermost.is_some_and(|c| starts_a_flow_collection(&c)) {
        return None;
    }
    Some(tab)
}

/// Whether `token` is white space holding a tab that separates an indicator
/// from a block collection.
///
/// A tab may separate an indicator from a scalar (`- \tx`) or a flow
/// collection (`seq:\t[]`), but a block collection after it needs
/// `s-indent`, which forbids tabs: `-\t-` and `?\t-` are errors the test
/// suite expects (Y79Y), while `-\t-1` is the scalar `-1`. The tab has to
/// separate the indicator from the node on the same line; trailing
/// whitespace before a line break (DC7X's `seq:\t`) or a comment
/// (`a:\t#`) indents nothing.
fn tab_before_block_collection(token: &crate::nodes::SyntaxToken) -> bool {
    use crate::SyntaxKind;
    if token.kind() != SyntaxKind::WHITESPACE || !token.text().contains('\t') {
        return false;
    }
    let Some(next) = token.next_sibling_or_token().and_then(|n| n.into_node()) else {
        return false;
    };
    let opens_a_block = |n: &SyntaxNode| {
        matches!(n.kind(), SyntaxKind::MAPPING | SyntaxKind::SEQUENCE)
            && !starts_a_flow_collection(n)
    };
    let collection = if opens_a_block(&next) {
        next
    } else if matches!(next.kind(), SyntaxKind::VALUE | SyntaxKind::KEY) {
        match next.children().find(opens_a_block) {
            Some(c) => c,
            None => return false,
        }
    } else {
        return false;
    };
    let first_entry = collection
        .children()
        .find(|c| {
            matches!(
                c.kind(),
                SyntaxKind::MAPPING_ENTRY | SyntaxKind::SEQUENCE_ENTRY
            )
        })
        .map_or(collection.text_range().start(), |e| e.text_range().start());
    !std::iter::successors(token.next_token(), |t| t.next_token())
        .take_while(|t| t.text_range().start() < first_entry)
        .any(|t| t.kind() == SyntaxKind::NEWLINE)
}

/// Whether `token` is an INDENT made of or starting with a tab, in a
/// collection that is flow if `in_flow`.
fn tab_indent(token: &crate::nodes::SyntaxToken, in_flow: bool) -> bool {
    if token.kind() != crate::SyntaxKind::INDENT {
        return false;
    }
    // A blank or comment-only line indents nothing, so tabs there are fine
    // (DK95/04, Y79Y/002).
    if token.next_token().map_or(true, |t| {
        matches!(
            t.kind(),
            crate::SyntaxKind::NEWLINE | crate::SyntaxKind::COMMENT
        )
    }) {
        return false;
    }
    // After leading spaces, or at the stream's own level where `s-indent`
    // is empty, a tab is separation rather than indentation (DK95/00's
    // `foo:\n \tbar`, 6HB6, Q5MG), unless a block collection entry
    // follows, whose column it would move (DK95/06). In a VALUE the next
    // sibling is the whole nested collection rather than its first entry
    // (`a:\n \t- x`).
    let starts_block_entry = !in_flow
        && token
            .next_sibling_or_token()
            .is_some_and(|n| match n.kind() {
                crate::SyntaxKind::MAPPING_ENTRY | crate::SyntaxKind::SEQUENCE_ENTRY => true,
                crate::SyntaxKind::MAPPING | crate::SyntaxKind::SEQUENCE => {
                    !n.as_node().is_some_and(starts_a_flow_collection)
                }
                _ => false,
            });
    let at_stream_level = token
        .parent()
        .is_some_and(|p| p.kind() == crate::SyntaxKind::DOCUMENT);
    if (token.text().starts_with(' ') || at_stream_level) && !starts_block_entry {
        return false;
    }
    token.text().contains('\t')
}

/// Check that an explicit key indicator is followed by white space or a
/// line break, as `c-mapping-key` needs. The lexer reads `{?, a}` as an
/// explicit key all the same, as nothing else could start there.
pub(super) fn check_explicit_key_separation(node: &SyntaxNode, violations: &mut Vec<Violation>) {
    use crate::SyntaxKind;

    for question in node
        .descendants_with_tokens()
        .filter_map(|c| c.into_token())
        .filter(|t| t.kind() == SyntaxKind::QUESTION)
    {
        let separated = std::iter::successors(question.next_token(), |t| t.next_token())
            .find(|t| !t.text().is_empty())
            .map_or(true, |t| {
                matches!(t.kind(), SyntaxKind::WHITESPACE | SyntaxKind::NEWLINE)
            });
        if !separated {
            violations.push(Violation::error_at(
                Rule::Other,
                question.text_range(),
                "Explicit key indicator '?' must be followed by white space".to_string(),
            ));
        }
    }
}

/// Check that the `:` of a block explicit entry that opens its line is at
/// the column of the entry's `?`, as `l-block-map-explicit-value` needs.
/// The parser takes one indented past it as the entry's value all the same
/// (`? a\n : b\n`).
pub(super) fn check_explicit_value_column(
    entry: &SyntaxNode,
    columns: &ColumnIndex,
    in_flow: bool,
    violations: &mut Vec<Violation>,
) {
    use crate::SyntaxKind;

    if in_flow {
        return;
    }
    let (Some(question), Some(colon)) = (
        child_token(entry, SyntaxKind::QUESTION),
        child_token(entry, SyntaxKind::COLON),
    ) else {
        return;
    };
    let opens_line = std::iter::successors(colon.prev_token(), |t| t.prev_token())
        .find(|t| {
            !t.text().is_empty() && !matches!(t.kind(), SyntaxKind::INDENT | SyntaxKind::WHITESPACE)
        })
        .map_or(true, |t| t.kind() == SyntaxKind::NEWLINE);
    if opens_line && columns.column(&colon) != columns.column(&question) {
        violations.push(Violation::error_at(
            Rule::Other,
            colon.text_range(),
            "Explicit value indicator ':' must be at the column of its '?'".to_string(),
        ));
    }
}
