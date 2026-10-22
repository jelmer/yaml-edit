//! Tags in the `tag:yaml.org,2002:` namespace, which can be written in the
//! `!!` shorthand (`!!set`) or verbatim (`!<tag:yaml.org,2002:set>`), and
//! the resolution of tag shorthands through `%TAG` directives.

use crate::lex::SyntaxKind;
use crate::yaml::SyntaxNode;
use std::borrow::Cow;

pub(crate) const STR: &str = "!!str";
pub(crate) const INT: &str = "!!int";
pub(crate) const FLOAT: &str = "!!float";
pub(crate) const BOOL: &str = "!!bool";
pub(crate) const NULL: &str = "!!null";
pub(crate) const MAP: &str = "!!map";
pub(crate) const SEQ: &str = "!!seq";
pub(crate) const SET: &str = "!!set";
pub(crate) const OMAP: &str = "!!omap";
pub(crate) const PAIRS: &str = "!!pairs";
#[cfg(feature = "base64")]
pub(crate) const BINARY: &str = "!!binary";
pub(crate) const TIMESTAMP: &str = "!!timestamp";
pub(crate) const REGEX: &str = "!!regex";

/// The name of `tag` within the `tag:yaml.org,2002:` namespace, such as
/// `set` for either spelling of the set tag.
pub(crate) fn yaml_org_name(tag: &str) -> Option<&str> {
    tag.strip_prefix("!!").or_else(|| verbatim_name(tag))
}

fn verbatim_name(tag: &str) -> Option<&str> {
    tag.strip_prefix("!<tag:yaml.org,2002:")?.strip_suffix('>')
}

/// `tag` in its `!!` shorthand when it is in the `tag:yaml.org,2002:`
/// namespace, so both spellings compare equal.
pub(crate) fn normalize(tag: &str) -> Cow<'_, str> {
    match verbatim_name(tag) {
        Some(name) => Cow::Owned(format!("!!{name}")),
        None => Cow::Borrowed(tag),
    }
}

/// The `%TAG` declarations in effect at `node`, as `(handle, prefix)`
/// pairs: those of the directives introducing its document, the last
/// declared first.
pub(crate) fn declared_handles(node: &SyntaxNode) -> Vec<(String, String)> {
    let Some(document) = node.ancestors().find(|n| n.kind() == SyntaxKind::DOCUMENT) else {
        return Vec::new();
    };
    document
        .siblings(rowan::Direction::Prev)
        .skip(1)
        .take_while(|n| n.kind() != SyntaxKind::DOCUMENT)
        .filter(|n| n.kind() == SyntaxKind::DIRECTIVE)
        .filter_map(|directive| {
            let text = directive.text().to_string();
            let mut words = text.split_whitespace();
            if words.next()? != "%TAG" {
                return None;
            }
            Some((words.next()?.to_string(), words.next()?.to_string()))
        })
        .collect()
}

/// `tag` as the `%TAG` declarations `handles` resolve it: in the `!!`
/// shorthand when it is in the `tag:yaml.org,2002:` namespace, verbatim
/// (`!<uri>`) when a declared handle expands it to another URI, and as
/// written otherwise.
pub(crate) fn resolve<'a>(tag: &'a str, handles: &[(String, String)]) -> Cow<'a, str> {
    let uri = shorthand(tag).and_then(|(handle, suffix)| {
        let (_, prefix) = handles.iter().find(|(h, _)| h == handle)?;
        Some(format!("{prefix}{suffix}"))
    });
    match uri {
        Some(uri) => match uri.strip_prefix("tag:yaml.org,2002:") {
            Some(name) => Cow::Owned(format!("!!{name}")),
            None => Cow::Owned(format!("!<{uri}>")),
        },
        None => normalize(tag),
    }
}

/// The handle and suffix of a tag shorthand such as `!e!x`, `!!x` or `!x`,
/// or `None` for a verbatim tag or the non-specific tag `!`.
fn shorthand(tag: &str) -> Option<(&str, &str)> {
    let rest = tag.strip_prefix('!')?;
    if rest.is_empty() || rest.starts_with('<') {
        return None;
    }
    Some(match rest.find('!') {
        Some(i) => (&tag[..i + 2], &rest[i + 1..]),
        None => ("!", rest),
    })
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_yaml_org_name() {
        assert_eq!(yaml_org_name("!!set"), Some("set"));
        assert_eq!(yaml_org_name("!<tag:yaml.org,2002:set>"), Some("set"));
        assert_eq!(yaml_org_name("!<tag:yaml.org,2002:set"), None);
        assert_eq!(yaml_org_name("!local"), None);
        assert_eq!(yaml_org_name("!<tag:example.com,2000:set>"), None);
    }

    #[test]
    fn test_resolve() {
        let handles = vec![
            ("!y!".to_string(), "tag:yaml.org,2002:".to_string()),
            ("!e!".to_string(), "tag:example.com,2000:".to_string()),
        ];
        assert_eq!(resolve("!y!int", &handles), "!!int");
        assert_eq!(resolve("!e!x", &handles), "!<tag:example.com,2000:x>");
        assert_eq!(resolve("!u!x", &handles), "!u!x");
        assert_eq!(resolve("!!int", &handles), "!!int");
        assert_eq!(resolve("!<tag:yaml.org,2002:int>", &handles), "!!int");
        assert_eq!(resolve("!", &handles), "!");
        assert_eq!(resolve("!local", &handles), "!local");

        let redefined = vec![("!!".to_string(), "tag:example.com,2000:".to_string())];
        assert_eq!(resolve("!!int", &redefined), "!<tag:example.com,2000:int>");
        let primary = vec![("!".to_string(), "tag:yaml.org,2002:".to_string())];
        assert_eq!(resolve("!int", &primary), "!!int");
        assert_eq!(resolve("!", &primary), "!");
    }

    #[test]
    fn test_normalize() {
        assert_eq!(normalize("!<tag:yaml.org,2002:omap>"), "!!omap");
        assert_eq!(normalize("!!omap"), "!!omap");
        assert_eq!(normalize("!local"), "!local");
        assert_eq!(normalize("!"), "!");
        assert_eq!(
            normalize("!<tag:example.com,2000:x>"),
            "!<tag:example.com,2000:x>"
        );
    }
}
