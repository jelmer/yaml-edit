use std::str::FromStr;
use yaml_edit::YamlFile;

#[test]
fn test_set_with_field_order_update_existing_key() {
    let original = r#"name: my-app
version: 1.0
description: A test app"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("version", 2.0, field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 2.0
description: A test app"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_insert_new_key_in_order() {
    let original = r#"name: my-app
description: A test app"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("version", 1.0, field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
description: A test app"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_insert_at_beginning() {
    let original = r#"version: 1.0
description: A test app"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("name", "my-app", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
description: A test app"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_insert_at_end() {
    let original = r#"name: my-app
version: 1.0"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("description", "A test app", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
description: A test app
"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_key_not_in_order() {
    let original = r#"name: my-app
version: 1.0"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description"];
            mapping.set_with_field_order("author", "John Doe", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
author: John Doe
"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_complex_ordering() {
    let original = r#"version: 1.0
author: John Doe
"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "description", "author"];

            // Add multiple keys in different orders
            mapping.set_with_field_order("description", "A test app", field_order);
            mapping.set_with_field_order("name", "my-app", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
description: A test app
author: John Doe
"#;
    assert_eq!(result, expected);
}

#[test]
fn test_document_set_with_field_order() {
    let original = r#"version: 1.0
author: John Doe
"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        let field_order = &["name", "version", "description", "author"];
        doc.set_with_field_order("name", "my-app", field_order);
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
version: 1.0
author: John Doe
"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_empty_document() {
    // Start with an empty mapping document
    let yaml = YamlFile::from_str("---\n{}").unwrap(); // Empty mapping

    if let Some(doc) = yaml.document() {
        let field_order = &["name", "version", "description"];
        doc.set_with_field_order("name", "my-app", field_order);
        doc.set_with_field_order("version", 1.0, field_order);
    }

    let result = yaml.to_string();
    assert_eq!(result, "---\n{}\nname: my-app\nversion: 1.0\n");
}

#[test]
fn test_insert_multiple_fields_before_all_existing() {
    // Issue 1: Test inserting multiple fields that should all come before existing fields
    let original = r#"d: value_d
e: value_e"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["a", "b", "c", "d", "e"];

            // Insert fields that should come before existing ones
            mapping.set_with_field_order("a", "value_a", field_order);
            mapping.set_with_field_order("b", "value_b", field_order);
            mapping.set_with_field_order("c", "value_c", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"a: value_a
b: value_b
c: value_c
d: value_d
e: value_e"#;
    assert_eq!(result, expected);
}

#[test]
fn test_no_extra_blank_lines_between_fields() {
    // Issue 2: Extra blank lines are sometimes added between fields
    let original = r#"a: value_a
c: value_c"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["a", "b", "c"];

            // Insert 'b' between 'a' and 'c'
            mapping.set_with_field_order("b", "value_b", field_order);
        }
    }

    let result = yaml.to_string();

    // Expected: No extra blank lines
    let expected = r#"a: value_a
b: value_b
c: value_c"#;

    assert_eq!(result, expected);
}

#[test]
fn test_no_extra_blank_lines_at_beginning() {
    // Test that inserting at the beginning doesn't add extra blank lines
    let original = r#"b: value_b
c: value_c"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["a", "b", "c"];
            mapping.set_with_field_order("a", "value_a", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"a: value_a
b: value_b
c: value_c"#;

    assert_eq!(result, expected);
}

#[test]
fn test_preserve_single_newline_style() {
    // Ensure that we preserve the original newline style and don't add extras
    let original = r#"a: value_a
c: value_c"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["a", "b", "c", "d"];
            mapping.set_with_field_order("b", "value_b", field_order);
            mapping.set_with_field_order("d", "value_d", field_order);
        }
    }

    let result = yaml.to_string();

    // Should have exactly 4 lines with no blank lines between them
    let expected = r#"a: value_a
b: value_b
c: value_c
d: value_d
"#;

    assert_eq!(result, expected);
}

#[test]
fn test_lintian_brush_rewrite_canonical() {
    // Real-world test case from lintian-brush
    // Input starts with "Archive:" but "Name" should be first in DEP-12 order
    let original = r#"Archive: pypi.python.org
Bug-Database: https://gitlab.com/fdroid/blah/issues
Bug-Submit: https://gitlab.com/fdroid/blah/issues/new
Cite-As: https://example.com
Contact: https://forum.example.com"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            // DEP-12 field order where Name comes first
            let field_order = &[
                "Name",
                "Contact",
                "Archive",
                "Bug-Database",
                "Bug-Submit",
                "Cite-As",
            ];

            // Add Name field which should be inserted at the very beginning
            mapping.set_with_field_order("Name", "blah", field_order);
        }
    }

    let result = yaml.to_string();

    // Name should be at the beginning, not somewhere in the middle
    let expected = r#"Name: blah
Archive: pypi.python.org
Bug-Database: https://gitlab.com/fdroid/blah/issues
Bug-Submit: https://gitlab.com/fdroid/blah/issues/new
Cite-As: https://example.com
Contact: https://forum.example.com"#;

    assert_eq!(
        result, expected,
        "Name should be inserted at the beginning before Archive"
    );
}

#[test]
fn test_multiline_value_no_extra_newlines() {
    // Simpler test: insert after a multiline value
    let original = r#"multi:
 - item1
 - item2
"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["multi", "next"];
            mapping.set_with_field_order("next", "value", field_order);
        }
    }

    let result = yaml.to_string();

    let expected = r#"multi:
 - item1
 - item2
next: value
"#;

    assert_eq!(result, expected);
}

#[test]
fn test_lintian_brush_line_interrupted() {
    // Real-world test case from lintian-brush
    // Multiple fields need to be inserted before "Registry" which is currently first
    let original = r#"Registry:
 - Name: conda:conda-forge
   Entry: r-tsne
"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &[
                "Name",
                "Contact",
                "Archive",
                "Bug-Database",
                "Bug-Submit",
                "Registry",
                "Repository",
                "Repository-Browse",
            ];

            // Add multiple fields that should all come before Registry
            mapping.set_with_field_order("Name", "tsne", field_order);
            mapping.set_with_field_order(
                "Contact",
                "Justin Donaldson <jdonaldson@gmail.com>",
                field_order,
            );
            mapping.set_with_field_order("Archive", "CRAN", field_order);
            mapping.set_with_field_order(
                "Bug-Database",
                "https://github.com/jdonaldson/rtsne/issues",
                field_order,
            );
            mapping.set_with_field_order(
                "Bug-Submit",
                "https://github.com/jdonaldson/rtsne/issues/new",
                field_order,
            );
            mapping.set_with_field_order(
                "Repository",
                "https://github.com/jdonaldson/rtsne.git",
                field_order,
            );
            mapping.set_with_field_order(
                "Repository-Browse",
                "https://github.com/jdonaldson/rtsne",
                field_order,
            );
        }
    }

    let result = yaml.to_string();

    // All new fields before Registry should be at the beginning, in order
    let expected = r#"Name: tsne
Contact: Justin Donaldson <jdonaldson@gmail.com>
Archive: CRAN
Bug-Database: https://github.com/jdonaldson/rtsne/issues
Bug-Submit: https://github.com/jdonaldson/rtsne/issues/new
Registry:
 - Name: conda:conda-forge
   Entry: r-tsne
Repository: https://github.com/jdonaldson/rtsne.git
Repository-Browse: https://github.com/jdonaldson/rtsne
"#;

    assert_eq!(result, expected);
}

#[test]
fn test_lintian_brush_security_md() {
    // Real-world test case from lintian-brush
    // Document starts with Repository, need to add fields before it
    let original = r#"---
Repository: https://github.com/example/blah.git
Repository-Browse: https://github.com/example/blah
"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &[
                "Name",
                "Bug-Database",
                "Bug-Submit",
                "Repository",
                "Repository-Browse",
                "Security-Contact",
            ];

            mapping.set_with_field_order("Name", "blah", field_order);
            mapping.set_with_field_order(
                "Bug-Database",
                "https://github.com/example/blah/issues",
                field_order,
            );
            mapping.set_with_field_order(
                "Bug-Submit",
                "https://github.com/example/blah/issues/new",
                field_order,
            );
            mapping.set_with_field_order(
                "Security-Contact",
                "https://github.com/example/blah/tree/HEAD/SECURITY.md",
                field_order,
            );
        }
    }

    let result = yaml.to_string();

    let expected = r#"---
Name: blah
Bug-Database: https://github.com/example/blah/issues
Bug-Submit: https://github.com/example/blah/issues/new
Repository: https://github.com/example/blah.git
Repository-Browse: https://github.com/example/blah
Security-Contact: https://github.com/example/blah/tree/HEAD/SECURITY.md
"#;

    assert_eq!(result, expected);
}

// Tests from test_set_with_field_order_extended.rs

#[test]
fn test_set_with_field_order_preserves_comments() {
    let original = r#"# Header comment
name: my-app  # inline comment
version: 1.0
# Footer comment"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "author", "version", "description"];
            mapping.set_with_field_order("author", "John Doe", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"# Header comment
name: my-app  # inline comment
author: John Doe
version: 1.0
# Footer comment"#
    );
}

#[test]
fn test_set_with_field_order_with_multiline_values() {
    let original = r#"name: my-app
description: |
  This is a long
  multiline description
  that spans multiple lines
version: 1.0"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "author", "description", "version"];
            mapping.set_with_field_order("author", "Jane Smith", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"name: my-app
author: Jane Smith
description: |
  This is a long
  multiline description
  that spans multiple lines
version: 1.0"#
    );
}

#[test]
fn test_set_with_field_order_duplicate_in_order() {
    let original = r#"name: my-app
version: 1.0"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "name", "author", "version", "version"];
            mapping.set_with_field_order("author", "Bob", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"name: my-app
author: Bob
version: 1.0"#
    );
}

#[test]
fn test_set_with_field_order_empty_field_order() {
    let original = r#"name: my-app
version: 1.0"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order: &[&str] = &[];
            mapping.set_with_field_order("author", "Alice", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"name: my-app
version: 1.0
author: Alice
"#
    );
}

#[test]
fn test_set_with_field_order_special_characters_in_keys() {
    let original = r#"regular-key: value1
"key with spaces": value2
'single-quoted': value3"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["regular-key", "new-key", "key with spaces", "single-quoted"];
            mapping.set_with_field_order("new-key", "new-value", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"regular-key: value1
new-key: new-value
"key with spaces": value2
'single-quoted': value3"#
    );
}

#[test]
fn test_set_with_field_order_nested_values() {
    let original = r#"name: my-app
config:
  host: localhost
  port: 8080
version: 1.0"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "author", "config", "version"];
            mapping.set_with_field_order("author", "Developer", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"name: my-app
author: Developer
config:
  host: localhost
  port: 8080
version: 1.0"#
    );
}

#[test]
fn test_set_with_field_order_update_with_different_value_type() {
    let original = r#"name: my-app
version: "1.0"
count: 42"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "count"];
            mapping.set_with_field_order("version", 2.0, field_order);
            mapping.set_with_field_order("count", 100, field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"name: my-app
version: 2.0
count: 100"#
    );
}

#[test]
fn test_set_with_field_order_single_field() {
    let original = r#"name: my-app
version: 1.0
description: test"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["version"];
            mapping.set_with_field_order("author", "Solo", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"name: my-app
version: 1.0
description: test
author: Solo
"#
    );
}

#[test]
fn test_set_with_field_order_all_new_fields() {
    // Note: Starting with "---\n{}" creates INVALID YAML when we add block entries
    // after the flow mapping. However, as a lossless parser, we preserve this
    // structure exactly as-is. Other YAML parsers will reject this as invalid.
    let yaml = YamlFile::from_str("---\n{}").unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "version", "author", "description"];

            mapping.set_with_field_order("description", "A test app", field_order);
            mapping.set_with_field_order("name", "test-app", field_order);
            mapping.set_with_field_order("author", "Tester", field_order);
            mapping.set_with_field_order("version", "0.1.0", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        "---\n{}\nname: test-app\nversion: 0.1.0\nauthor: Tester\ndescription: A test app\n"
    );
}

#[test]
fn test_set_with_field_order_partial_order_match() {
    let original = r#"name: my-app
random: value
version: 1.0
another: data"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["name", "author", "version"];
            mapping.set_with_field_order("author", "Partial", field_order);
        }
    }

    let result = yaml.to_string();
    let expected = r#"name: my-app
author: Partial
random: value
version: 1.0
another: data"#;
    assert_eq!(result, expected);
}

#[test]
fn test_set_with_field_order_long_field_order() {
    let original = r#"start: value
end: value"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &[
                "field1", "field2", "field3", "field4", "field5", "start", "middle", "field6",
                "field7", "field8", "field9", "field10", "end", "field11", "field12",
            ];
            mapping.set_with_field_order("middle", "inserted", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"start: value
middle: inserted
end: value"#
    );
}

#[test]
fn test_set_with_field_order_unicode_keys_and_values() {
    let original = r#"名前: アプリ
version: 1.0"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["名前", "作者", "version"];
            mapping.set_with_field_order("作者", "山田太郎", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"名前: アプリ
作者: 山田太郎
version: 1.0"#
    );
}

#[test]
fn test_set_with_field_order_multiple_operations_complex() {
    let original = r#"field1: value1
field5: value5"#;

    let yaml = YamlFile::from_str(original).unwrap();

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            let field_order = &["field1", "field2", "field3", "field4", "field5"];

            mapping.set_with_field_order("field3", "value3", field_order);
            mapping.set_with_field_order("field2", "value2", field_order);
            mapping.set_with_field_order("field4", "value4", field_order);
            mapping.set_with_field_order("field1", "updated1", field_order);
        }
    }

    let result = yaml.to_string();
    assert_eq!(
        result,
        r#"field1: updated1
field2: value2
field3: value3
field4: value4
field5: value5"#
    );
}
