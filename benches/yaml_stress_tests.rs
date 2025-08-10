use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::str::FromStr;
use yaml_edit::Yaml;

fn bench_block_scalar_stress_test(c: &mut Criterion) {
    let yaml = r#"
app:
  name: "Complex App"
  
  documentation:
    overview: >
      This application demonstrates complex
      YAML structures with multiple block
      scalar styles and nested content.
    
    installation: |
      Step 1: Download the package
      Step 2: Extract to /opt/app
      Step 3: Run ./install.sh
      
      Note: Requires sudo privileges.
    
    examples:
      - name: "Basic Usage"
        code: |
          import app
          
          client = app.Client()
          client.connect()
          result = client.process(data)
      
      - name: "Advanced Usage"  
        description: >
          This example shows advanced features
          including error handling and logging.
        code: |
          import app
          import logging
          
          logging.basicConfig(level=logging.INFO)
          
          try:
              client = app.Client(
                  host="localhost",
                  port=8080,
                  timeout=30
              )
              
              with client:
                  for item in data:
                      result = client.process(item)
                      print(f"Processed: {result}")
                      
          except app.ConnectionError as e:
              logging.error(f"Connection failed: {e}")
          except Exception as e:
              logging.error(f"Unexpected error: {e}")

  configuration:
    database: |
      # Database Configuration
      host: localhost
      port: 5432
      database: myapp
      
      pool:
        min_connections: 5
        max_connections: 20
        timeout: 30
    
    logging: |+
      version: 1
      
      formatters:
        default:
          format: '[%(asctime)s] %(levelname)s: %(message)s'
      
      handlers:
        console:
          class: logging.StreamHandler
          formatter: default
          level: INFO
          
        file:
          class: logging.FileHandler
          filename: app.log
          formatter: default
          level: DEBUG
      
      root:
        level: DEBUG
        handlers: [console, file]


"#;

    c.bench_function("block_scalar_stress_test", |b| {
        b.iter(|| {
            let parsed = Yaml::from_str(black_box(yaml)).unwrap();
            black_box(parsed.to_string())
        })
    });
}

fn bench_block_scalars_in_nested_structures(c: &mut Criterion) {
    let yaml = r#"config:
  database:
    connection_string: |
      host=localhost
      port=5432
      dbname=myapp
      user=admin
      password=secret
    
    migration_script: >
      This is a long migration script that
      spans multiple lines but should be
      treated as a single folded string.
  
  logging:
    format: |
      [%timestamp%] %level%: %message%
      Additional context: %context%
    
    rules:
      - name: "Error Rule"
        pattern: >
          This pattern matches error messages
          that span multiple lines in the log file.
      
      - name: "Warning Rule"  
        pattern: |
          ^WARNING:.*
          (.*continuation.*)*
"#;

    c.bench_function("block_scalars_in_nested_structures", |b| {
        b.iter(|| {
            let parsed = Yaml::from_str(black_box(yaml)).unwrap();
            black_box(parsed.to_string())
        })
    });
}

fn bench_block_scalar_sequence_interaction(c: &mut Criterion) {
    let yaml = r#"items:
  - description: |
      This is a multi-line
      description for the first item.
      
      It has multiple paragraphs.
    
    value: 123
  
  - description: >
      This is a folded description
      for the second item that
      should be on one line.
    
    value: 456

  - simple_value

  - |
    A literal block scalar
    as a sequence item directly.
    
  - >
    A folded block scalar
    as a sequence item directly.
"#;

    c.bench_function("block_scalar_sequence_interaction", |b| {
        b.iter(|| {
            let parsed = Yaml::from_str(black_box(yaml)).unwrap();
            black_box(parsed.to_string())
        })
    });
}

criterion_group!(
    benches,
    bench_block_scalar_stress_test,
    bench_block_scalars_in_nested_structures,
    bench_block_scalar_sequence_interaction
);
criterion_main!(benches);
