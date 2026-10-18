use std::path::{Path, PathBuf};
use std::process::{Command, Output};

/// A fresh directory holding `current`, `base` and `other` files.
fn setup(name: &str, current: &str, base: &str, other: &str) -> PathBuf {
    let dir = Path::new(env!("CARGO_TARGET_TMPDIR")).join(name);
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).unwrap();
    for (file, text) in [("current", current), ("base", base), ("other", other)] {
        std::fs::write(dir.join(file), text).unwrap();
    }
    dir
}

fn run(dir: &Path, args: &[&str]) -> Output {
    Command::new(env!("CARGO_BIN_EXE_yaml-merge3"))
        .current_dir(dir)
        .args(args)
        .args(["current", "base", "other"])
        .output()
        .unwrap()
}

fn read(dir: &Path, file: &str) -> String {
    std::fs::read_to_string(dir.join(file)).unwrap()
}

fn stderr(output: &Output) -> String {
    String::from_utf8(output.stderr.clone()).unwrap()
}

fn stdout(output: &Output) -> String {
    String::from_utf8(output.stdout.clone()).unwrap()
}

#[test]
fn clean_merge_overwrites_current() {
    let dir = setup("clean", "a: 2\nb: 1\n", "a: 1\nb: 1\n", "a: 1\nb: 3\n");
    let output = run(&dir, &[]);
    assert_eq!(output.status.code(), Some(0));
    assert_eq!(stdout(&output), "");
    assert_eq!(stderr(&output), "");
    assert_eq!(read(&dir, "current"), "a: 2\nb: 3\n");
}

#[test]
fn stdout_leaves_current_alone() {
    let dir = setup("stdout", "a: 2\nb: 1\n", "a: 1\nb: 1\n", "a: 1\nb: 3\n");
    let output = run(&dir, &["-p"]);
    assert_eq!(output.status.code(), Some(0));
    assert_eq!(stdout(&output), "a: 2\nb: 3\n");
    assert_eq!(read(&dir, "current"), "a: 2\nb: 1\n");
}

#[test]
fn conflicts_set_the_exit_code_and_are_reported() {
    let dir = setup(
        "conflicts",
        "s:\n  host: b\nl:\n  - a\n  - x\n",
        "s:\n  host: a\nl:\n  - a\n  - b\n",
        "s:\n  host: c\nl:\n  - a\n  - y\n",
    );
    let output = run(&dir, &["-p"]);
    assert_eq!(output.status.code(), Some(2));
    assert_eq!(stderr(&output), "conflict in s.host\nconflict in l[1]\n");
    assert_eq!(
        stdout(&output),
        concat!(
            "s:\n",
            "<<<<<<< current\n  host: b\n||||||| base\n  host: a\n=======\n  host: c\n>>>>>>> other\n",
            "l:\n  - a\n",
            "<<<<<<< current\n  - x\n||||||| base\n  - b\n=======\n  - y\n>>>>>>> other\n",
        )
    );
}

#[test]
fn labels_marker_size_and_quiet() {
    let dir = setup("labels", "a: 2\n", "a: 1\n", "a: 3\n");
    let output = run(
        &dir,
        &["-q", "-L", "mine", "-L", "old", "--marker-size", "3"],
    );
    assert_eq!(output.status.code(), Some(1));
    assert_eq!(stderr(&output), "");
    assert_eq!(
        read(&dir, "current"),
        "<<< mine\na: 2\n||| old\na: 1\n===\na: 3\n>>> other\n"
    );
}

#[test]
fn document_level_conflict() {
    let dir = setup("document", "b\n", "a\n", "c\n");
    let output = run(&dir, &["-p"]);
    assert_eq!(output.status.code(), Some(1));
    assert_eq!(stderr(&output), "conflict at document level\n");
}

#[test]
fn errors_exit_255() {
    let dir = setup("errors", "a: 1\n", "a: 1\n", "a: 1\n");
    let output = run(&dir, &["-L", "a", "-L", "b", "-L", "c", "-L", "d"]);
    assert_eq!(output.status.code(), Some(255));
    assert_eq!(stderr(&output), "yaml-merge3: too many labels\n");

    std::fs::remove_file(dir.join("base")).unwrap();
    let output = run(&dir, &[]);
    assert_eq!(output.status.code(), Some(255));
    let missing = std::fs::read_to_string(dir.join("base")).unwrap_err();
    assert_eq!(stderr(&output), format!("yaml-merge3: base: {missing}\n"));
    assert_eq!(read(&dir, "current"), "a: 1\n");
}
