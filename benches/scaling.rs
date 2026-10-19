//! Operations that once took time quadratic in the size of their input.
//!
//! Each benchmark runs at size n and 4n: a linear operation takes about 4
//! times as long at the larger size, a quadratic one about 16 times.

use criterion::{black_box, criterion_group, criterion_main, BatchSize, BenchmarkId, Criterion};
use rowan::ast::AstNode;
use std::str::FromStr;
use yaml_edit::validator::Validator;
use yaml_edit::{Document, Parse, YamlFile, YamlValue};

/// Benchmark `run` on the input `setup` builds for sizes `n` and `4 * n`.
fn bench_sizes<T>(
    c: &mut Criterion,
    name: &str,
    n: usize,
    setup: impl Fn(usize) -> T,
    run: impl Fn(&T),
) {
    let mut group = c.benchmark_group(name);
    for size in [n, 4 * n] {
        let input = setup(size);
        group.bench_with_input(BenchmarkId::from_parameter(size), &input, |b, input| {
            b.iter(|| run(black_box(input)))
        });
    }
    group.finish();
}

fn bench_parsing(c: &mut Criterion) {
    // Each entry of the innermost sequence scanned the rest of the input
    // for the colon of an implicit key.
    bench_sizes(
        c,
        "parse/nested_open_brackets",
        1000,
        |n| "[".repeat(n),
        |text| {
            Parse::<YamlFile>::parse_yaml(text);
        },
    );
}

/// Benchmark validating the text `build` gives for sizes `n` and `4 * n`.
fn bench_validation(c: &mut Criterion, name: &str, n: usize, build: impl Fn(usize) -> String) {
    bench_sizes(
        c,
        &format!("validate/{name}"),
        n,
        |n| YamlFile::from_str(&build(n)).unwrap(),
        |file| {
            Validator::new().validate_syntax(file.syntax());
        },
    );
}

fn bench_validator(c: &mut Criterion) {
    // This check used to compare every key against every other one, which
    // made validating a large generated mapping quadratic.
    bench_validation(c, "duplicate_keys", 500, |n| {
        (0..n).map(|i| format!("key{i}: value{i}\n")).collect()
    });

    // Each node used to look at all of its ancestors, or at all of its
    // subtree, and the parser allows up to 256 levels.
    bench_validation(c, "nested_flow_mappings", 60, |n| {
        format!("{}{}", "{".repeat(n), "}".repeat(n))
    });
    bench_validation(c, "nested_flow_sequences", 60, |n| {
        format!("{}{}", "[".repeat(n), "]".repeat(n))
    });
    bench_validation(c, "nested_flow_mapping_keys", 60, |n| {
        let keys: String = (0..n).map(|i| format!(": 1, a{i}: 2}}")).collect();
        format!("{}x{keys}", "{".repeat(n))
    });
    bench_validation(c, "nested_block_sequences", 60, |n| {
        format!("{}x\n", "- ".repeat(n))
    });

    // Each mapping worked out the identities of all the keys nested in
    // its own two collection keys again. Each level nests twice.
    bench_validation(c, "nested_sequence_keys", 30, |n| {
        format!("{}x{}", "{[0]: 1, [".repeat(n), "]: 1}".repeat(n))
    });
    bench_validation(c, "nested_mapping_keys", 30, |n| {
        format!("{}x{}", "{{0: 1}: 1, {a: ".repeat(n), "}: 1}".repeat(n))
    });

    // Each multi-line scalar looked through all its ancestors for the
    // block entry its continuation lines have to clear.
    bench_validation(c, "nested_multiline_keys", 60, |n| {
        format!("{}x{}", "{\"a\nb\": ".repeat(n), "}".repeat(n))
    });
    bench_validation(c, "nested_multiline_entries", 60, |n| {
        format!("k: {}x{}", "[\"a\n b\", ".repeat(n), "]".repeat(n))
    });

    // Each implicit key was searched for a line break through all the
    // keys nested in it.
    bench_validation(c, "nested_implicit_keys", 60, |n| {
        format!("{}x{}", "[".repeat(n), "]: 1".repeat(n))
    });

    // Finding a token's column walked back to the start of its line.
    bench_validation(c, "line_length", 500, |n| {
        let pairs: Vec<String> = (0..n).map(|i| format!("k{i}: v")).collect();
        format!("[{}]\n", pairs.join(", "))
    });

    // The column of each violation was counted from its line's start.
    bench_validation(c, "violations", 2000, |n| {
        format!("[{}]\n", vec!["@a"; n].join(", "))
    });

    // Locating violations indexed the whole stream for every document,
    // even one without any.
    bench_sizes(
        c,
        "validate/each_document",
        500,
        |n| YamlFile::from_str(&"---\na: 1\n".repeat(n)).unwrap(),
        |file| {
            for doc in file.documents() {
                Validator::new().validate(&doc);
            }
        },
    );
}

/// Benchmark converting the text `build` gives for sizes `n` and `4 * n`.
fn bench_conversion(c: &mut Criterion, name: &str, n: usize, build: impl Fn(usize) -> String) {
    bench_sizes(
        c,
        &format!("convert/{name}"),
        n,
        |n| YamlFile::from_str(&build(n)).unwrap(),
        |file| {
            YamlValue::from_document(&file.document().unwrap()).unwrap();
        },
    );
}

fn bench_converter(c: &mut Criterion) {
    // Each key was compared with every earlier one of its mapping.
    bench_conversion(c, "mapping_keys", 2000, |n| {
        (0..n).map(|i| format!("key{i}: value{i}\n")).collect()
    });
    // Walking a node of the editable tree costs its depth.
    bench_conversion(c, "nested_flow_mappings", 60, |n| {
        let keys: String = (0..n).map(|i| format!("{{k{i}: ")).collect();
        format!("{keys}x{}", "}".repeat(n))
    });
}

fn bench_validate_tree(c: &mut Criterion) {
    // Walking a node of the editable tree costs its depth.
    bench_sizes(
        c,
        "validate_tree/nested_flow_mappings",
        60,
        |n| {
            let keys: String = (0..n).map(|i| format!("{{k{i}: ")).collect();
            YamlFile::from_str(&format!("{keys}x{}", "}".repeat(n))).unwrap()
        },
        |file| yaml_edit::debug::validate_tree(file.syntax()).unwrap(),
    );
    // Each entry looked through all its ancestors for a flow collection.
    bench_sizes(
        c,
        "validate_tree/nested_block_sequences",
        60,
        |n| YamlFile::from_str(&format!("{}x\n", "- ".repeat(n))).unwrap(),
        |file| yaml_edit::debug::validate_tree(file.syntax()).unwrap(),
    );
}

fn bench_sequence_push(c: &mut Criterion) {
    // `push` used to hold every child of the SEQUENCE alive in a Vec across
    // `splice_children`, which has to fix up each live SyntaxNode handle.
    // That made one push cost O(len) twice over and building a sequence
    // O(len^2): 800 pushes took over a second.
    let mut group = c.benchmark_group("sequence_push");
    for size in [375, 1500] {
        group.bench_function(BenchmarkId::from_parameter(size), |b| {
            b.iter_batched(
                || Document::from_str("items:\n  - a\n").unwrap(),
                |doc| {
                    let seq = doc.as_mapping().unwrap().get_sequence("items").unwrap();
                    for i in 0..size {
                        seq.push(format!("v{i}"));
                    }
                    doc
                },
                BatchSize::SmallInput,
            )
        });
    }
    group.finish();
}

criterion_group!(
    benches,
    bench_parsing,
    bench_validator,
    bench_converter,
    bench_validate_tree,
    bench_sequence_push
);
criterion_main!(benches);
