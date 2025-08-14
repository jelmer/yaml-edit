//! Example demonstrating the YAML builder pattern API.

use yaml_edit::{MappingBuilder, SequenceBuilder, YamlBuilder};

fn main() {
    println!("=== Simple Scalar ===");
    let scalar_yaml = YamlBuilder::scalar("Hello, YAML!").build();
    println!("{}", scalar_yaml);

    println!("\n=== Simple Sequence ===");
    let sequence_yaml = YamlBuilder::sequence()
        .item("apple")
        .item("banana")
        .item("cherry")
        .build()
        .build();
    println!("{}", sequence_yaml);

    println!("\n=== Simple Mapping ===");
    let mapping_yaml = YamlBuilder::mapping()
        .pair("name", "Alice")
        .pair("age", "30")
        .pair("city", "San Francisco")
        .build()
        .build();
    println!("{}", mapping_yaml);

    println!("\n=== Complex Nested Structure ===");
    let complex_yaml = YamlBuilder::mapping()
        .pair("apiVersion", "v1")
        .pair("kind", "ConfigMap")
        .mapping("metadata", |m| {
            m.pair("name", "app-config").pair("namespace", "production")
        })
        .mapping("data", |m| {
            m.pair("database.url", "postgresql://localhost:5432/myapp")
                .pair("cache.ttl", "3600")
                .sequence("features", |s| {
                    s.item("authentication")
                        .item("notifications")
                        .item("analytics")
                })
        })
        .build()
        .build();
    println!("{}", complex_yaml);

    println!("\n=== Server Configuration ===");
    let server_config = YamlBuilder::mapping()
        .mapping("server", |m| {
            m.pair("host", "0.0.0.0")
                .pair("port", "8080")
                .sequence("middleware", |s| {
                    s.item("cors").item("compression").item("logging")
                })
        })
        .mapping("database", |m| {
            m.pair("driver", "postgres").mapping("connection", |c| {
                c.pair("host", "db.example.com")
                    .pair("port", "5432")
                    .pair("database", "production")
                    .pair("ssl", "true")
            })
        })
        .sequence("workers", |s| {
            s.mapping(|m| m.pair("name", "worker-1").pair("threads", "4"))
                .mapping(|m| m.pair("name", "worker-2").pair("threads", "4"))
        })
        .build()
        .build();
    println!("{}", server_config);

    println!("\n=== Deeply Nested Example ===");
    let deep_yaml = YamlBuilder::mapping()
        .mapping("level1", |m| {
            m.pair("key1", "value1").mapping("level2", |m| {
                m.pair("key2", "value2").mapping("level3", |m| {
                    m.pair("key3", "value3").mapping("level4", |m| {
                        m.pair("key4", "value4")
                            .sequence("items", |s| s.item("deep1").item("deep2").item("deep3"))
                    })
                })
            })
        })
        .build()
        .build();
    println!("{}", deep_yaml);
}
