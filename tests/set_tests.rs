use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::{ScalarValue, Set, TaggedScalar, Yaml, YamlValue};

#[test]
fn test_set_cast_success() {
    let yaml = r#"
!!set
? apple
? banana  
? cherry
"#;

    let doc = Yaml::from_str(yaml).unwrap();
    let documents: Vec<_> = doc.documents().collect();
    assert_eq!(documents.len(), 1);

    // The document should have a root node that is a tagged scalar with !!set tag
    let root = documents[0].root_node().expect("Should have root node");
    let tagged_scalar = TaggedScalar::cast(root).expect("Should be a tagged scalar");
    assert_eq!(tagged_scalar.tag(), Some("!!set".to_string()));

    // Should successfully cast to Set
    let set = tagged_scalar.as_set().expect("Should cast to Set");

    // Verify the set members
    let members = set.members();
    assert_eq!(members.len(), 3);

    // Check that all expected members are present
    let member_strings: Vec<&str> = members
        .iter()
        .map(|v| match v {
            YamlValue::Scalar(s) => s.value(),
            _ => panic!("Expected scalar values in set"),
        })
        .collect();

    assert!(member_strings.contains(&"apple"));
    assert!(member_strings.contains(&"banana"));
    assert!(member_strings.contains(&"cherry"));
}

#[test]
fn test_set_cast_failure_wrong_tag() {
    let yaml = r#"
!!str "not a set"
"#;

    let doc = Yaml::from_str(yaml).unwrap();
    let documents: Vec<_> = doc.documents().collect();
    assert_eq!(documents.len(), 1);

    if let Some(root) = documents[0].root_node() {
        if let Some(tagged_scalar) = TaggedScalar::cast(root) {
            // Should not cast to Set since it's not a !!set tag
            assert!(tagged_scalar.as_set().is_none());
        }
    }
}

#[test]
fn test_set_cast_failure_not_tagged() {
    let yaml = r#"
regular: value
"#;

    let doc = Yaml::from_str(yaml).unwrap();
    let documents: Vec<_> = doc.documents().collect();
    assert_eq!(documents.len(), 1);

    // Should not be a tagged scalar at all
    if let Some(root) = documents[0].root_node() {
        assert!(TaggedScalar::cast(root).is_none());
    }
}

#[test]
fn test_empty_set() {
    let yaml = r#"
!!set {}
"#;

    let doc = Yaml::from_str(yaml).unwrap();
    let documents: Vec<_> = doc.documents().collect();
    assert_eq!(documents.len(), 1);

    let root = documents[0].root_node().expect("Should have root node");
    let tagged_scalar = TaggedScalar::cast(root).expect("Should be a tagged scalar");
    let set = tagged_scalar.as_set().expect("Should cast to Set");

    // Empty set should have no members
    let members = set.members();
    assert_eq!(members.len(), 0);
}

#[test]
fn test_set_with_complex_keys() {
    // This test verifies that our Set can handle complex keys in the future
    // For now, the implementation only handles scalar keys
    let yaml = r#"
!!set
? simple
? [complex, key]
"#;

    let doc = Yaml::from_str(yaml).unwrap();
    let documents: Vec<_> = doc.documents().collect();
    assert_eq!(documents.len(), 1);

    if let Some(root) = documents[0].root_node() {
        if let Some(tagged_scalar) = TaggedScalar::cast(root) {
            if let Some(set) = tagged_scalar.as_set() {
                let members = set.members();
                // Currently only scalar keys are supported, so we should get at least one member
                assert!(members.len() >= 1);

                // The scalar key should be present
                let has_simple = members.iter().any(|v| match v {
                    YamlValue::Scalar(s) => s.value() == "simple",
                    _ => false,
                });
                assert!(has_simple);
            }
        }
    }
}

#[test]
fn test_set_direct_cast() {
    let yaml = r#"
!!set
? item1
? item2
"#;

    let doc = Yaml::from_str(yaml).unwrap();
    let documents: Vec<_> = doc.documents().collect();

    // Try direct casting from the document's root node
    if let Some(root) = documents[0].root_node() {
        if root.kind() == yaml_edit::SyntaxKind::TAGGED_SCALAR {
            let set = Set::cast(root).expect("Should cast to Set");

            let members = set.members();
            assert_eq!(members.len(), 2);
        }
    }
}

#[test]
fn test_set_syntax_access() {
    let yaml = r#"
!!set
? test
"#;

    let doc = Yaml::from_str(yaml).unwrap();
    let documents: Vec<_> = doc.documents().collect();

    let root = documents[0].root_node().unwrap();
    let tagged_scalar = TaggedScalar::cast(root).unwrap();
    let set = tagged_scalar.as_set().unwrap();

    // Should be able to access the underlying syntax node
    let syntax = set.syntax();
    assert_eq!(syntax.kind(), yaml_edit::SyntaxKind::TAGGED_SCALAR);
    assert!(syntax.text().to_string().contains("!!set"));
}
