//! Tags in the `tag:yaml.org,2002:` namespace, which can be written in the
//! `!!` shorthand (`!!set`) or verbatim (`!<tag:yaml.org,2002:set>`).

use std::borrow::Cow;

pub(crate) const STR: &str = "!!str";
pub(crate) const INT: &str = "!!int";
pub(crate) const FLOAT: &str = "!!float";
pub(crate) const BOOL: &str = "!!bool";
pub(crate) const NULL: &str = "!!null";
pub(crate) const MAP: &str = "!!map";
pub(crate) const SEQ: &str = "!!seq";
pub(crate) const SET: &str = "!!set";

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
