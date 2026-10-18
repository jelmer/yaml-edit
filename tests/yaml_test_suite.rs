//! YAML Test Suite integration tests.
//!
//! Runs tests from the official YAML Test Suite: https://github.com/yaml/yaml-test-suite
//!
//! ## Usage
//!
//! - Run all tests: `cargo test --test yaml_test_suite`
//! - Run first N tests: `LIMIT=100 cargo test --test yaml_test_suite`
//! - Verbose output: `VERBOSE=1 cargo test --test yaml_test_suite`
//!
//! ## Note on println!()
//!
//! This test file intentionally uses `println!()` for progress reporting and test harness output.
//! This is an exception to the general "no println!()" rule for tests, as this file functions
//! as a test harness/runner that reports on YAML test suite compliance.

use rowan::ast::AstNode;
use serde_json::Value;
use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use yaml_edit::anchor_resolution::AnchorRegistry;
use yaml_edit::validator::{Severity, Validator};
use yaml_edit::{Scalar, ScalarStyle, ScalarType, ScalarValue, SyntaxKind, YamlFile, YamlNode};

/// Represents a single test case from the YAML Test Suite.
#[derive(Debug)]
struct TestCase {
    /// Test ID (directory name)
    id: String,
    /// Test name/description from === file
    name: String,
    /// Path to the test directory
    path: PathBuf,
    /// Whether this test should produce an error
    should_error: bool,
}

impl TestCase {
    fn load(path: PathBuf) -> Option<Self> {
        let id = path.file_name()?.to_str()?.to_string();

        // Read test name from === file
        let name_file = path.join("===");
        let name = if name_file.exists() {
            fs::read_to_string(&name_file).ok()?.trim().to_string()
        } else {
            id.clone()
        };

        // Check if test should produce an error
        let should_error = path.join("error").exists();

        Some(TestCase {
            id,
            name,
            path,
            should_error,
        })
    }

    fn input_yaml(&self) -> Option<String> {
        let input_file = self.path.join("in.yaml");
        if input_file.exists() {
            fs::read_to_string(&input_file).ok()
        } else {
            None
        }
    }
}

/// Collects all test cases from the test-data directory.
///
/// Handles both single-part tests (with `in.yaml` directly in the directory)
/// and multi-part tests (with numbered subdirectories like `00/`, `01/`, etc.).
/// Skips metadata directories like `name/` and `tags/`.
fn collect_tests() -> Vec<TestCase> {
    let test_data_dir = Path::new("test-data");

    if !test_data_dir.exists() {
        println!("Test data not found at {:?}", test_data_dir);
        println!("Run: git clone --depth 1 -b data https://github.com/yaml/yaml-test-suite.git test-data/");
        return Vec::new();
    }

    // Metadata directories that are not test cases
    const SKIP_DIRS: &[&str] = &["name", "tags"];

    let mut tests = Vec::new();

    if let Ok(entries) = fs::read_dir(test_data_dir) {
        for entry in entries.flatten() {
            let path = entry.path();
            if !path.is_dir() {
                continue;
            }
            let dir_name = match path.file_name().and_then(|n| n.to_str()) {
                Some(name) => name.to_string(),
                None => continue,
            };
            if SKIP_DIRS.contains(&dir_name.as_str()) {
                continue;
            }

            if path.join("in.yaml").exists() || path.join("===").exists() {
                // Single-part test
                if let Some(test) = TestCase::load(path) {
                    tests.push(test);
                }
            } else {
                // Multi-part test: look for numbered subdirectories
                if let Ok(sub_entries) = fs::read_dir(&path) {
                    for sub_entry in sub_entries.flatten() {
                        let sub_path = sub_entry.path();
                        if sub_path.is_dir() && sub_path.join("in.yaml").exists() {
                            let sub_name = sub_path
                                .file_name()
                                .and_then(|n| n.to_str())
                                .unwrap_or("??");
                            let composite_id = format!("{}/{}", dir_name, sub_name);
                            if let Some(mut test) = TestCase::load(sub_path) {
                                test.id = composite_id;
                                tests.push(test);
                            }
                        }
                    }
                }
            }
        }
    }

    tests.sort_by(|a, b| a.id.cmp(&b.id));
    tests
}

/// Valid cases that the validator still reports errors for. A listed case
/// that validates cleanly fails the run, so drop it here once it is fixed.
#[rustfmt::skip]
const KNOWN_VALIDATOR_FALSE_POSITIVES: &[&str] = &[
    // Duplicate null keys, which the validator reports deliberately; the
    // suite only checks syntax.
    "2JQS",
];

#[derive(Debug, Default)]
struct TestResults {
    passed: usize,
    failed: usize,
    skipped: usize,
}

impl TestResults {
    fn report(&self) {
        let total = self.passed + self.failed + self.skipped;
        println!("\n=== YAML Test Suite Results ===");
        println!("Total tests: {}", total);
        println!(
            "Passed: {} ({:.1}%)",
            self.passed,
            self.passed as f64 / total as f64 * 100.0
        );
        println!(
            "Failed: {} ({:.1}%)",
            self.failed,
            self.failed as f64 / total as f64 * 100.0
        );
        println!(
            "Skipped: {} ({:.1}%)",
            self.skipped,
            self.skipped as f64 / total as f64 * 100.0
        );
    }
}

fn run_test(test: &TestCase, verbose: bool) -> bool {
    let input = match test.input_yaml() {
        Some(input) => input,
        None => {
            if verbose {
                println!("SKIP {}: No in.yaml file", test.id);
            }
            return true; // Skip tests without input
        }
    };

    // Use catch_unwind to prevent infinite loops or panics from hanging the test suite
    // Parse with YamlFile to get the full tree including directives
    let parse_result = std::panic::catch_unwind(|| YamlFile::from_str(&input));

    let success = match parse_result {
        Ok(Ok(yaml)) if !test.should_error => {
            use rowan::ast::AstNode;
            if let Err(e) = yaml_edit::debug::validate_tree(yaml.syntax()) {
                println!("FAIL {}: Tree violates CST invariants: {}", test.id, e);
                return false;
            }
            // A valid document should also pass the validator.
            let errors: Vec<_> = Validator::new()
                .validate_syntax(yaml.syntax())
                .into_iter()
                .filter(|v| v.severity == Severity::Error)
                .collect();
            let known = KNOWN_VALIDATOR_FALSE_POSITIVES.contains(&test.id.as_str());
            match (errors.is_empty(), known) {
                (true, false) => {
                    if verbose {
                        println!("PASS {}: Successfully parsed and validated", test.id);
                    }
                    true
                }
                (false, true) => {
                    if verbose {
                        println!("PASS {}: Known validator false positive", test.id);
                    }
                    true
                }
                (true, true) => {
                    println!(
                        "FAIL {}: Listed as a known validator false positive but validates cleanly",
                        test.id
                    );
                    false
                }
                (false, false) => {
                    println!("FAIL {}: Validator rejected a valid case", test.id);
                    println!("  Name: {}", test.name);
                    for violation in &errors {
                        println!("    {}", violation);
                    }
                    false
                }
            }
        }
        Ok(Err(_)) if test.should_error => {
            // Test expects error and got error - pass
            if verbose {
                println!("PASS {}: Correctly rejected invalid YAML", test.id);
            }
            true
        }
        Ok(Ok(yaml)) if test.should_error => {
            // Lenient parser accepted it, but test expects error
            // Check if validator catches it as a spec violation
            // Use validate_syntax on the root to catch directive-level issues
            let validator = Validator::new();

            let violations = validator.validate_syntax(yaml.syntax());

            if !violations.is_empty() {
                // Validator caught the spec violation - pass
                if verbose {
                    println!(
                        "PASS {}: Validator caught {} spec violation(s)",
                        test.id,
                        violations.len()
                    );
                    for violation in &violations {
                        println!("    {}", violation);
                    }
                }
                true
            } else {
                // Neither parser nor validator caught the error - fail
                println!("FAIL {}: Expected error but parsed successfully", test.id);
                println!("  Name: {}", test.name);
                false
            }
        }
        Ok(Err(err)) if !test.should_error => {
            // Test expects success but got error - fail
            if verbose {
                println!("FAIL {}: Parse error: {}", test.id, err);
                println!("  Name: {}", test.name);
            }
            false
        }
        Err(_) => {
            // Parser panicked
            println!("PANIC {}: Parser panicked or hung", test.id);
            println!("  Name: {}", test.name);
            false
        }
        _ => unreachable!(),
    };

    success
}

#[test]
fn yaml_test_suite() {
    let tests = collect_tests();

    if tests.is_empty() {
        panic!(
            "No tests found. Clone test data with:\n\
                git clone --depth 1 -b data https://github.com/yaml/yaml-test-suite.git test-data/"
        );
    }

    let verbose = std::env::var("VERBOSE").is_ok();
    let limit: Option<usize> = std::env::var("LIMIT").ok().and_then(|s| s.parse().ok());

    let test_count = if let Some(lim) = limit {
        println!(
            "Running first {} of {} tests from YAML Test Suite",
            lim,
            tests.len()
        );
        lim.min(tests.len())
    } else {
        println!("Running {} tests from YAML Test Suite", tests.len());
        tests.len()
    };

    let mut results = TestResults::default();

    for (i, test) in tests.iter().take(test_count).enumerate() {
        if i % 50 == 0 {
            println!("Progress: {}/{}", i, test_count);
        }
        if run_test(test, verbose) {
            results.passed += 1;
        } else {
            results.failed += 1;
        }
    }

    results.report();

    // Fail the test on any regression. The suite currently passes 100%
    // of its cases; any new failure means either yaml-edit broke a case
    // it used to handle, or the harness's coarse pass criteria (see
    // `run_test`) started letting through a genuine bug. Investigate,
    // don't downgrade the assertion.
    assert_eq!(
        results.failed, 0,
        "{} YAML test suite case(s) failed. Run with VERBOSE=1 for details, LIMIT=N to narrow.",
        results.failed,
    );
}

/// Valid cases whose parsed values do not match `in.json`, with the reason.
///
/// Keep this sorted. Once a case is fixed, `yaml_test_suite_values` fails
/// until it is removed from here.
const KNOWN_VALUE_FAILURES: &[(&str, &str)] = &[("3GZX", "redefined anchor")];

/// Converts parsed documents to the JSON that `in.json` holds.
struct JsonConverter {
    anchors: AnchorRegistry,
    /// Whether `!!` still means the core schema tags, i.e. no `%TAG !!`
    /// directive redefines it.
    core_tags: bool,
}

impl JsonConverter {
    fn node(&self, node: &YamlNode) -> Result<Value, String> {
        match node {
            YamlNode::Scalar(scalar) => self.scalar(scalar, None),
            YamlNode::Mapping(mapping) => {
                let mut object = serde_json::Map::new();
                for entry in mapping.entries() {
                    let key = match entry.key_node() {
                        None => String::new(),
                        Some(key) => self.key(&key)?,
                    };
                    let value = match entry.value_node() {
                        None => Value::Null,
                        Some(value) => self.node(&value)?,
                    };
                    object.insert(key, value);
                }
                Ok(Value::Object(object))
            }
            YamlNode::Sequence(sequence) => sequence
                .values()
                .map(|item| self.node(&item))
                .collect::<Result<_, _>>()
                .map(Value::Array),
            YamlNode::Alias(alias) => self.node(&self.resolve_alias(&alias.name())?),
            YamlNode::TaggedNode(tagged) => {
                let tag = tagged.tag();
                if let Some(mapping) = tagged.as_mapping() {
                    self.node(&YamlNode::Mapping(mapping))
                } else if let Some(sequence) = tagged.as_sequence() {
                    self.node(&YamlNode::Sequence(sequence))
                } else if let Some(scalar) = tagged.value() {
                    self.scalar(&scalar, tag.as_deref())
                } else {
                    Err(format!("tagged node {:?} has no value", tag))
                }
            }
        }
    }

    fn resolve_alias(&self, name: &str) -> Result<YamlNode, String> {
        let node = self
            .anchors
            .resolve(name)
            .ok_or_else(|| format!("undefined alias *{}", name))?;
        // Anchors on mapping values point at the VALUE wrapper.
        let node = if node.kind() == SyntaxKind::VALUE {
            node.first_child()
        } else {
            Some(node.clone())
        };
        node.and_then(YamlNode::from_syntax)
            .ok_or_else(|| format!("anchor &{} has no value", name))
    }

    /// JSON keys are strings, so scalar keys use their string value as written.
    fn key(&self, key: &YamlNode) -> Result<String, String> {
        match key {
            YamlNode::Scalar(scalar) => Ok(scalar.as_string()),
            YamlNode::TaggedNode(tagged) => tagged
                .as_string()
                .ok_or_else(|| format!("non-scalar key {}", key)),
            YamlNode::Alias(alias) => self.key(&self.resolve_alias(&alias.name())?),
            _ => Err(format!("non-scalar key {}", key)),
        }
    }

    /// Resolve a scalar per the YAML 1.2 core schema, honouring its tags.
    fn scalar(&self, scalar: &Scalar, tag: Option<&str>) -> Result<Value, String> {
        let value = ScalarValue::from_scalar(scalar);
        let core_tag = tag.filter(|_| self.core_tags);
        let scalar_type = match core_tag {
            Some("!!str") => ScalarType::String,
            Some("!!int") => ScalarType::Integer,
            Some("!!float") => ScalarType::Float,
            Some("!!bool") => ScalarType::Boolean,
            Some("!!null") => ScalarType::Null,
            _ if tag == Some("!") => ScalarType::String,
            _ if value.style() == ScalarStyle::Plain && value.value().is_empty() => {
                ScalarType::Null
            }
            _ => value.scalar_type(),
        };
        let invalid = || format!("{:?} is not a valid {:?}", value.value(), scalar_type);
        let coerced = value.coerce_to_type(scalar_type).ok_or_else(invalid)?;
        match scalar_type {
            ScalarType::Null => Ok(Value::Null),
            ScalarType::Boolean => coerced.to_bool().map(Value::Bool).ok_or_else(invalid),
            ScalarType::Integer => coerced.to_i64().map(Value::from).ok_or_else(invalid),
            ScalarType::Float => {
                let float = coerced.to_f64().ok_or_else(invalid)?;
                serde_json::Number::from_f64(float)
                    .map(Value::Number)
                    .ok_or_else(|| format!("{} has no JSON equivalent", value.value()))
            }
            _ => Ok(Value::String(value.value().to_string())),
        }
    }
}

/// Like `==`, but numbers compare by value so that `1.0` matches `1`.
fn json_eq(a: &Value, b: &Value) -> bool {
    match (a, b) {
        (Value::Number(a), Value::Number(b)) => a.as_f64() == b.as_f64(),
        (Value::Array(a), Value::Array(b)) => {
            a.len() == b.len() && a.iter().zip(b).all(|(a, b)| json_eq(a, b))
        }
        (Value::Object(a), Value::Object(b)) => {
            a.len() == b.len()
                && a.iter()
                    .all(|(key, a)| b.get(key).is_some_and(|b| json_eq(a, b)))
        }
        _ => a == b,
    }
}

/// Checks that `input` parses to the documents in `expected`, which may
/// hold several concatenated JSON values, one per document.
fn check_values(input: &str, expected: &[Value]) -> Result<(), String> {
    let file = YamlFile::from_str(input).map_err(|err| format!("parse error: {}", err))?;
    let core_tags = !file
        .directives()
        .any(|d| d.is_tag() && d.value().is_some_and(|v| v.starts_with("!! ")));
    let mut actual = Vec::new();
    for doc in file.documents() {
        let converter = JsonConverter {
            anchors: AnchorRegistry::from_document(&doc),
            core_tags,
        };
        actual.push(
            match doc.syntax().children().find_map(YamlNode::from_syntax) {
                None => Value::Null,
                Some(root) => converter.node(&root)?,
            },
        );
    }
    if actual.len() == expected.len() && actual.iter().zip(expected).all(|(a, e)| json_eq(a, e)) {
        Ok(())
    } else {
        Err(format!(
            "expected {}, got {}",
            Value::Array(expected.to_vec()),
            Value::Array(actual)
        ))
    }
}

/// Compares the values of every valid case that has an `in.json` against it.
///
/// JSON cannot express tags, so only the core schema tags are honoured, to
/// resolve the scalars they apply to. Mapping keys are compared by their
/// string value. Cases without an `in.json` (mostly those with non-scalar
/// keys) are not checked.
#[test]
fn yaml_test_suite_values() {
    let tests = collect_tests();
    assert!(!tests.is_empty(), "No tests found, see yaml_test_suite");
    assert!(
        KNOWN_VALUE_FAILURES.windows(2).all(|w| w[0].0 < w[1].0),
        "KNOWN_VALUE_FAILURES is not sorted"
    );

    let mut checked = 0;
    let mut problems = Vec::new();
    for test in tests.iter().filter(|t| !t.should_error) {
        let json_path = test.path.join("in.json");
        let Some(input) = test.input_yaml().filter(|_| json_path.exists()) else {
            continue;
        };
        let expected_json = fs::read_to_string(&json_path)
            .unwrap_or_else(|err| panic!("{}: cannot read in.json: {}", test.id, err));
        let expected = serde_json::Deserializer::from_str(&expected_json)
            .into_iter()
            .collect::<Result<Vec<Value>, _>>()
            .unwrap_or_else(|err| panic!("{}: invalid in.json: {}", test.id, err));
        let known = KNOWN_VALUE_FAILURES.iter().any(|(id, _)| *id == test.id);
        let outcome = std::panic::catch_unwind(|| check_values(&input, &expected))
            .unwrap_or_else(|_| Err("panicked".to_string()));
        match (outcome, known) {
            (Ok(()), false) | (Err(_), true) => {}
            (Ok(()), true) => problems.push(format!(
                "{}: unexpectedly passed, remove from KNOWN_VALUE_FAILURES",
                test.id
            )),
            (Err(detail), false) => {
                problems.push(format!("{} ({}): {}", test.id, test.name, detail))
            }
        }
        checked += 1;
    }

    println!(
        "Compared values for {} cases ({} known failures)",
        checked,
        KNOWN_VALUE_FAILURES.len()
    );
    assert!(
        problems.is_empty(),
        "{} YAML test suite value check(s) failed:\n{}",
        problems.len(),
        problems.join("\n")
    );
}

/// Inputs on which fuzzing found yaml-edit reading other values than the
/// YAML spec gives, as `(name, input, expected)`, `expected` holding one JSON
/// value per document.
const FUZZ_FOUND_VALUES: &[(&str, &str, &str)] = &[
    (
        "anchor-on-own-line",
        "a:\n &x\nb: 1\n",
        r#"{"a": null, "b": 1}"#,
    ),
    (
        "bom-before-indented-sequence",
        "\u{feff} - &a\n - b\n",
        r#"[null, "b"]"#,
    ),
    (
        "comment-after-plain-continuation",
        "1\n|x\n  # t\n",
        r#""1 |x""#,
    ),
    (
        "flow-explicit-key-on-next-line",
        "[?\n1 ]\n",
        r#"[{"1": null}]"#,
    ),
    (
        "flow-indicator-in-plain-before-document-marker",
        "f\n[\n---\n",
        r#""f [" null"#,
    ),
    (
        "flow-indicator-in-plain-key-after-colon",
        ":o [d: f\n",
        r#"{":o [d": "f"}"#,
    ),
    ("single-quoted-cr-line-break", "' a\r b'\n", r#"" a b""#),
    (
        "tab-line-after-block-scalar",
        "foo: |\n  x\n \t\nbr: 1\n",
        r#"{"foo": "x\n", "br": 1}"#,
    ),
];

/// Names in `FUZZ_FOUND_VALUES` that are not read correctly yet. Once one
/// is fixed, `fuzz_found_values` fails until it is removed from here.
const KNOWN_FUZZ_FOUND_FAILURES: &[&str] = &[
    "anchor-on-own-line",
    "bom-before-indented-sequence",
    "comment-after-plain-continuation",
    "flow-explicit-key-on-next-line",
    "flow-indicator-in-plain-before-document-marker",
    "flow-indicator-in-plain-key-after-colon",
    "tab-line-after-block-scalar",
];

#[test]
fn fuzz_found_values() {
    let mut problems = Vec::new();
    for (name, input, expected_json) in FUZZ_FOUND_VALUES {
        let expected = serde_json::Deserializer::from_str(expected_json)
            .into_iter()
            .collect::<Result<Vec<Value>, _>>()
            .unwrap_or_else(|err| panic!("{name}: invalid JSON: {err}"));
        let known = KNOWN_FUZZ_FOUND_FAILURES.contains(name);
        match (check_values(input, &expected), known) {
            (Ok(()), false) | (Err(_), true) => {}
            (Ok(()), true) => problems.push(format!(
                "{name}: unexpectedly passed, remove from KNOWN_FUZZ_FOUND_FAILURES"
            )),
            (Err(detail), false) => problems.push(format!("{name} {input:?}: {detail}")),
        }
    }
    assert!(problems.is_empty(), "{}", problems.join("\n"));
}
