//! Rewriting YAML 1.1 constructs into their YAML 1.2 spellings.
//!
//! Two plain-scalar forms changed meaning between the versions, and both
//! change it silently: neither is a syntax error under either version.
//!
//! - A bare leading zero was octal, so `0755` was 493 and is now 755.
//! - `yes`, `no`, `on` and `off` were booleans, and are now plain strings.
//!
//! [`to_yaml_1_2`] rewrites both so the document keeps the value a YAML 1.1
//! reader saw: `0755` becomes `0o755` and `yes` becomes `true`. Everything
//! else, formatting and comments included, is left alone.
//!
//! [`crate::validator::Rule::LegacyYaml11`] reports the same constructs
//! without changing anything, for callers who would rather decide per site.

use crate::lex::SyntaxKind;
use crate::yaml::{Document, SyntaxNode};
use rowan::ast::AstNode;

/// A scalar that [`to_yaml_1_2`] rewrote.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Rewrite {
    /// The text as the document had it.
    pub before: String,
    /// The text it was replaced with.
    pub after: String,
}

/// Rewrite the YAML 1.1 constructs in `doc` to their 1.2 spellings,
/// returning what changed.
///
/// The value a YAML 1.1 reader saw is preserved, which is the reason to
/// migrate at all: `mode: 0755` meant 493 and becomes `mode: 0o755`, which
/// still means 493. `flag: yes` meant true and becomes `flag: true`.
///
/// A quoted scalar is already unambiguous, so it is never touched.
///
/// # Example
///
/// ```rust
/// # use std::str::FromStr;
/// # use yaml_edit::Document;
/// let doc = Document::from_str("mode: 0755\nflag: yes\n").unwrap();
/// let rewrites = yaml_edit::migrate::to_yaml_1_2(&doc);
///
/// assert_eq!(doc.to_string(), "mode: 0o755\nflag: true\n");
/// assert_eq!(rewrites.len(), 2);
/// ```
///
/// Mutates in place despite `&doc` (see crate docs on interior mutability).
pub fn to_yaml_1_2(doc: &Document) -> Vec<Rewrite> {
    let mut rewrites = Vec::new();
    rewrite_in(doc.syntax(), &mut rewrites);
    rewrites
}

fn rewrite_in(node: &SyntaxNode, rewrites: &mut Vec<Rewrite>) {
    for child in node.children() {
        if child.kind() == SyntaxKind::SCALAR {
            if let Some((after, kind)) = replacement_for(&child) {
                let before = child.text().to_string();
                let count = child.children_with_tokens().count();
                child.splice_children(
                    0..count,
                    vec![crate::nodes::fresh_token(kind, &after).into()],
                );
                rewrites.push(Rewrite { before, after });
            }
            continue;
        }
        rewrite_in(&child, rewrites);
    }
}

/// The 1.2 spelling of `scalar`, with the token kind it should carry, or
/// `None` when the scalar is already unambiguous.
fn replacement_for(scalar: &SyntaxNode) -> Option<(String, SyntaxKind)> {
    // A scalar holding anything but its own value token (a block scalar's
    // header, say) is not a plain scalar to reinterpret.
    let mut tokens = scalar.children_with_tokens();
    let token = tokens.next()?.into_token()?;
    if tokens.next().is_some() {
        return None;
    }
    if !matches!(
        token.kind(),
        SyntaxKind::INT | SyntaxKind::STRING | SyntaxKind::BOOL
    ) {
        return None;
    }

    let text = token.text();
    // Quoting already settles the reading.
    if text.starts_with(['"', '\'']) {
        return None;
    }

    // So does an explicit tag: `!!str yes` is the string in both versions,
    // and `!!int 0755` is whatever the tag says it is.
    if scalar
        .parent()
        .is_some_and(|p| p.kind() == SyntaxKind::TAGGED_NODE)
    {
        return None;
    }

    if crate::ScalarValue::is_legacy_octal(text) {
        let sign = if text.starts_with('-') { "-" } else { "" };
        let digits = text.strip_prefix(['+', '-']).unwrap_or(text);
        // `08` is no octal at all, so there is no 1.1 reading to preserve.
        i64::from_str_radix(digits, 8).ok()?;
        return Some((
            format!("{sign}0o{}", digits.trim_start_matches('0')),
            SyntaxKind::INT,
        ));
    }

    match text {
        "yes" | "Yes" | "YES" | "on" | "On" | "ON" => Some(("true".into(), SyntaxKind::BOOL)),
        "no" | "No" | "NO" | "off" | "Off" | "OFF" => Some(("false".into(), SyntaxKind::BOOL)),
        _ => None,
    }
}
