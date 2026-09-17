//! `CustomTagHandler` is public API, so its signature has to be nameable.

use yaml_edit::custom_tags::{CustomTagError, CustomTagHandler};
use yaml_edit::{ScalarValue, YamlValue};

struct Upper;

impl CustomTagHandler for Upper {
    fn serialize(&self, value: &YamlValue) -> Result<String, CustomTagError> {
        match value {
            YamlValue::Scalar(s) => Ok(s.value().to_uppercase()),
            _ => Err(CustomTagError::new("!upper", "not a scalar")),
        }
    }

    fn deserialize(&self, content: &str) -> Result<YamlValue, CustomTagError> {
        Ok(YamlValue::Scalar(ScalarValue::string(content)))
    }

    fn description(&self) -> &str {
        "uppercase"
    }
}

/// The trait names `YamlValue`, so that type has to be exported too: while it
/// was not, no caller outside the crate could write this impl at all.
#[test]
fn a_handler_can_be_implemented_outside_the_crate() {
    let handler = Upper;
    let value = YamlValue::Scalar(ScalarValue::string("abc"));
    assert_eq!(handler.serialize(&value).unwrap(), "ABC");
    assert_eq!(
        handler.deserialize("xyz").unwrap(),
        YamlValue::Scalar(ScalarValue::string("xyz"))
    );
    assert_eq!(handler.description(), "uppercase");
}
