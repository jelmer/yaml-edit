use yaml_edit::{Yaml, YamlValue};
use std::str::FromStr;

#[test]
fn test_nested_mutation_debug() {
    let yaml_str = r#"database:
  name: dev_db
  user: admin"#;

    let mut yaml = Yaml::from_str(yaml_str).unwrap();
    println!("Initial YAML:\n{}", yaml.to_string());

    if let Some(doc) = yaml.document() {
        if let Some(mapping) = doc.as_mapping() {
            println!("\nGot root mapping");
            // Try direct get first
            let key_val = yaml_edit::YamlValue::from("database");
            let get_result = mapping.get(&key_val);
            println!("get() result: {:?}", get_result.is_some());
            if let Some(node) = &get_result {
                println!("Node kind: {:?}", node.kind());
                println!("Node text: {}", node.text());
            }
            
            let db_result = mapping.get_mapping(&YamlValue::scalar("database"));
            println!("get_mapping result: {:?}", db_result.is_some());
            if let Some(mut db) = db_result {
                println!("Got database mapping");
                
                db.set(&YamlValue::scalar("name"), &YamlValue::scalar("prod_db"));
                println!("Set name to prod_db");
                
                // Check the db mapping directly
                println!("\nDatabase mapping after modification:");
                println!("{}", db.to_string());
            }
        }
    }

    println!("\nFinal YAML from root:");
    println!("{}", yaml.to_string());
    
    let expected = r#"database:
  name: prod_db
  user: admin"#;
    assert_eq!(yaml.to_string(), expected);
}