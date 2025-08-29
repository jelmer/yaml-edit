use yaml_edit::{Yaml, YamlValue};

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn debug_nested_mutation() {
        let yaml_str = r#"
server:
  host: localhost
  port: 8080
  config:
    debug: true
    timeout: 30
database:
  host: db.example.com
  port: 5432
"#.trim();

        let mut yaml = Yaml::from_str(yaml_str).unwrap();
        
        println!("Original:");
        println!("{}", yaml.to_string());

        // Test nested mutations propagate to root
        if let Some(doc) = yaml.document() {
            if let Some(mapping) = doc.as_mapping() {
                println!("Got root mapping");
                
                // Modify deeply nested value
                if let Some(mut server) = mapping.get_mapping(&YamlValue::scalar("server")) {
                    println!("Got server mapping");
                    
                    if let Some(mut config) = server.get_mapping(&YamlValue::scalar("config")) {
                        println!("Got config mapping");
                        
                        config.set(&YamlValue::scalar("debug"), &YamlValue::scalar("false"));
                        config.set(&YamlValue::scalar("timeout"), &YamlValue::scalar("60"));
                        config.set(&YamlValue::scalar("new_option"), &YamlValue::scalar("enabled"));
                        
                        println!("Modified config mapping");
                    }
                }

                // Modify another branch
                if let Some(mut database) = mapping.get_mapping(&YamlValue::scalar("database")) {
                    println!("Got database mapping");
                    
                    database.set(&YamlValue::scalar("host"), &YamlValue::scalar("prod-db.example.com"));
                    database.set(&YamlValue::scalar("ssl"), &YamlValue::scalar("true"));
                    
                    println!("Modified database mapping");
                }
            }
        }

        // Verify all changes are visible in final output
        let output = yaml.to_string();
        println!("\nFinal output:");
        println!("{}", output);
        
        println!("\nChecking for expected values:");
        println!("Contains 'debug: false': {}", output.contains("debug: false"));
        println!("Contains 'timeout: 60': {}", output.contains("timeout: 60"));
        println!("Contains 'new_option: enabled': {}", output.contains("new_option: enabled"));
        println!("Contains 'host: prod-db.example.com': {}", output.contains("host: prod-db.example.com"));
        println!("Contains 'ssl: true': {}", output.contains("ssl: true"));
    }
}