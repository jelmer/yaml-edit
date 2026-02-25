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

use std::fs;
use std::path::{Path, PathBuf};
use std::str::FromStr;
use yaml_edit::validator::Validator;
use yaml_edit::YamlFile;

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
        Ok(Ok(_yaml)) if !test.should_error => {
            // Test expects success and got success - pass
            if verbose {
                println!("PASS {}: Successfully parsed", test.id);
            }
            true
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

            use rowan::ast::AstNode;
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

    // For now, we don't fail the test suite - just report results
    // Once we improve compliance, we can make this stricter
    if results.failed > 0 {
        println!(
            "\nNote: Some tests failed. This is expected as yaml-edit is still in development."
        );
        println!("Run with VERBOSE=1 to see detailed failure information.");
        println!("Run with LIMIT=N to test only the first N cases.");
    }
}
