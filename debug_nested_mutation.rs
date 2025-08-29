use yaml_edit::{Yaml, YamlValue};

fn main() {
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
    
    // Try to modify nested values
    if let Some(doc) = yaml.document() {
        if let Some(mut mapping) = doc.as_mapping() {
            println!("Got root mapping");
            
            // Get server mapping
            if let Some(server_value) = mapping.get(&YamlValue::scalar("server")) {
                println!("Got server value: {:?}", server_value);
                
                // If it's a mapping, try to modify its config
                if let YamlValue::Mapping(mut server_mapping) = server_value.clone() {
                    println!("Server is a mapping");
                    
                    if let Some(config_value) = server_mapping.get(&YamlValue::scalar("config")) {
                        println!("Got config value: {:?}", config_value);
                        
                        if let YamlValue::Mapping(mut config_mapping) = config_value.clone() {
                            println!("Config is a mapping");
                            config_mapping.insert(YamlValue::scalar("debug"), YamlValue::scalar("false"));
                            config_mapping.insert(YamlValue::scalar("timeout"), YamlValue::scalar("60"));
                            
                            // Put the modified config back
                            server_mapping.insert(YamlValue::scalar("config"), YamlValue::Mapping(config_mapping));
                        }
                    }
                    
                    // Put the modified server back  
                    mapping.set(&YamlValue::scalar("server"), &YamlValue::Mapping(server_mapping));
                }
            }
        }
    }
    
    println!("\nModified:");
    println!("{}", yaml.to_string());
}