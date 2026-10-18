use yaml_merge3::{merge, Conflict, MergeOptions};

fn clean(base: &str, ours: &str, theirs: &str) -> String {
    let result = merge(base, ours, theirs, &MergeOptions::default());
    assert_eq!(
        result.conflicts,
        vec![],
        "unexpected conflict:\n{}",
        result.text
    );
    result.text
}

fn conflicted(base: &str, ours: &str, theirs: &str) -> (String, Vec<Vec<String>>) {
    let result = merge(base, ours, theirs, &MergeOptions::default());
    let paths = result.conflicts.into_iter().map(|c| c.path).collect();
    (result.text, paths)
}

fn path(keys: &[&str]) -> Vec<String> {
    keys.iter().map(|k| k.to_string()).collect()
}

#[test]
fn adjacent_keys_changed_on_each_side() {
    assert_eq!(
        clean(
            "a: 1\nb: 2\nc: 3\n",
            "a: 1\nb: 20\nc: 3\n",
            "a: 1\nb: 2\nc: 30\n"
        ),
        "a: 1\nb: 20\nc: 30\n"
    );
}

#[test]
fn unchanged_on_one_side() {
    assert_eq!(clean("a: 1\n", "a: 1\n", "a: 2\n"), "a: 2\n");
    assert_eq!(clean("a: 1\n", "a: 2\n", "a: 1\n"), "a: 2\n");
}

#[test]
fn same_change_on_both_sides() {
    assert_eq!(
        clean("a: 1\nb: 1\n", "a: 2\nb: 1\n", "a: 2\nb: 1\n"),
        "a: 2\nb: 1\n"
    );
}

#[test]
fn same_value_spelled_differently_keeps_ours() {
    assert_eq!(clean("a: 1\n", "a: 'x'\n", "a: \"x\"\n"), "a: 'x'\n");
}

#[test]
fn conflicting_scalar_change() {
    let (text, paths) = conflicted(
        "a: 1\nb: 2\nc: 3\n",
        "a: 1\nb: 20\nc: 3\n",
        "a: 1\nb: 200\nc: 3\n",
    );
    assert_eq!(
        text,
        "a: 1\n<<<<<<<\nb: 20\n|||||||\nb: 2\n=======\nb: 200\n>>>>>>>\nc: 3\n"
    );
    assert_eq!(paths, vec![path(&["b"])]);
}

#[test]
fn conflict_labels_and_marker_size() {
    let options = MergeOptions {
        ours_label: Some("HEAD".into()),
        base_label: Some("base".into()),
        theirs_label: Some("topic".into()),
        marker_size: 3,
    };
    let result = merge("a: 1\n", "a: 2\n", "a: 3\n", &options);
    assert_eq!(
        result.text,
        "<<< HEAD\na: 2\n||| base\na: 1\n===\na: 3\n>>> topic\n"
    );
}

#[test]
fn additions_on_both_sides() {
    assert_eq!(
        clean("a: 1\nz: 9\n", "a: 1\nb: 2\nz: 9\n", "a: 1\nz: 9\ny: 8\n"),
        "a: 1\nb: 2\nz: 9\ny: 8\n"
    );
}

#[test]
fn addition_placed_after_preceding_key() {
    assert_eq!(
        clean("a: 1\nc: 3\n", "a: 10\nc: 3\nd: 4\n", "a: 1\nb: 2\nc: 3\n"),
        "a: 10\nb: 2\nc: 3\nd: 4\n"
    );
}

#[test]
fn addition_at_start() {
    assert_eq!(clean("b: 2\n", "b: 20\n", "a: 1\nb: 2\n"), "a: 1\nb: 20\n");
}

#[test]
fn same_key_added_with_different_values() {
    let (text, paths) = conflicted("a: 1\n", "a: 1\nb: 2\n", "a: 1\nb: 3\n");
    assert_eq!(
        text,
        "a: 1\n<<<<<<<\nb: 2\n|||||||\n=======\nb: 3\n>>>>>>>\n"
    );
    assert_eq!(paths, vec![path(&["b"])]);
}

#[test]
fn same_mapping_added_on_both_sides_merges() {
    assert_eq!(
        clean("a: 1\n", "a: 1\nb:\n  x: 1\n", "a: 1\nb:\n  y: 2\n"),
        "a: 1\nb:\n  x: 1\n  y: 2\n"
    );
}

#[test]
fn deletion() {
    assert_eq!(
        clean("a: 1\nb: 2\nc: 3\n", "a: 1\nb: 2\nc: 30\n", "a: 1\nc: 3\n"),
        "a: 1\nc: 30\n"
    );
    assert_eq!(clean("a: 1\nb: 2\n", "a: 1\n", "a: 10\nb: 2\n"), "a: 10\n");
}

#[test]
fn deleted_by_them_modified_by_us() {
    let (text, paths) = conflicted("a: 1\nb: 2\n", "a: 1\nb: 3\n", "a: 1\n");
    assert_eq!(
        text,
        "a: 1\n<<<<<<<\nb: 3\n|||||||\nb: 2\n=======\n>>>>>>>\n"
    );
    assert_eq!(paths, vec![path(&["b"])]);
}

#[test]
fn deleted_by_us_modified_by_them() {
    let (text, paths) = conflicted("a: 1\nb: 2\nc: 3\n", "a: 1\nc: 3\n", "a: 1\nb: 4\nc: 3\n");
    assert_eq!(
        text,
        "a: 1\n<<<<<<<\n|||||||\nb: 2\n=======\nb: 4\n>>>>>>>\nc: 3\n"
    );
    assert_eq!(paths, vec![path(&["b"])]);
}

#[test]
fn nested_mappings_merge_key_by_key() {
    let base = "server:\n  host: localhost\n  port: 80\nname: x\n";
    let ours = "server:\n  host: example.com\n  port: 80\nname: x\n";
    let theirs = "server:\n  host: localhost\n  port: 8080\nname: x\n";
    assert_eq!(
        clean(base, ours, theirs),
        "server:\n  host: example.com\n  port: 8080\nname: x\n"
    );
}

#[test]
fn nested_conflict_is_narrow() {
    let base = "server:\n  host: a\n  port: 80\n";
    let ours = "server:\n  host: b\n  port: 80\n";
    let theirs = "server:\n  host: c\n  port: 81\n";
    let (text, paths) = conflicted(base, ours, theirs);
    assert_eq!(
        text,
        "server:\n<<<<<<<\n  host: b\n|||||||\n  host: a\n=======\n  host: c\n>>>>>>>\n  port: 81\n"
    );
    assert_eq!(paths, vec![path(&["server", "host"])]);
}

#[test]
fn comments_and_formatting_preserved() {
    let base = "# config\na: 1   # first\n\n# about b\nb: 'two'\n";
    let ours = "# config\na: 1   # first\n\n# about b\nb: \"TWO\"  # changed\n";
    let theirs = "# config\na: 2   # first\n\n# about b\nb: 'two'\n";
    assert_eq!(
        clean(base, ours, theirs),
        "# config\na: 2   # first\n\n# about b\nb: \"TWO\"  # changed\n"
    );
}

#[test]
fn comment_above_entry_and_value_change_combine() {
    let base = "a: 1\nb: 2\n";
    let ours = "a: 1\n# explain b\nb: 2\n";
    let theirs = "a: 1\nb: 3\n";
    assert_eq!(clean(base, ours, theirs), "a: 1\n# explain b\nb: 3\n");
}

#[test]
fn parse_errors_elsewhere_are_kept() {
    let base = "a: 1\nbroken: [1, 2\nc: 3\n";
    let ours = "a: 10\nbroken: [1, 2\nc: 3\n";
    let theirs = "a: 1\nbroken: [1, 2\nc: 30\n";
    let result = merge(base, ours, theirs, &MergeOptions::default());
    assert_eq!(result.text, "a: 10\nbroken: [1, 2\nc: 30\n");
}

#[test]
fn list_changes_on_different_items() {
    let base = "l:\n  - a\n  - b\n";
    assert_eq!(
        clean(base, "l:\n  - x\n  - b\n", "l:\n  - a\n  - b\n  - c\n"),
        "l:\n  - x\n  - b\n  - c\n"
    );
    assert_eq!(
        clean(base, "l:\n  - b\n", "l:\n  - a\n  - b\n  - c\n"),
        "l:\n  - b\n  - c\n"
    );
}

#[test]
fn list_appends_on_both_sides() {
    assert_eq!(
        clean("l:\n  - a\n", "l:\n  - a\n  - b\n", "l:\n  - a\n  - c\n"),
        "l:\n  - a\n  - b\n  - c\n"
    );
}

#[test]
fn list_same_item_added_on_both_sides_appears_once() {
    assert_eq!(
        clean(
            "l:\n  - a\n",
            "l:\n  - a\n  - b\n  - c\n",
            "l:\n  - a\n  - c\n  - d\n"
        ),
        "l:\n  - a\n  - b\n  - c\n  - d\n"
    );
}

#[test]
fn list_inserts_at_different_places() {
    assert_eq!(
        clean(
            "l:\n  - a\n  - b\n  - c\n",
            "l:\n  - x\n  - a\n  - b\n  - c\n",
            "l:\n  - a\n  - b\n  - y\n  - c\n"
        ),
        "l:\n  - x\n  - a\n  - b\n  - y\n  - c\n"
    );
}

#[test]
fn list_item_changed_differently() {
    let (text, paths) = conflicted(
        "l:\n  - a\n  - b\n  - c\n",
        "l:\n  - a\n  - x\n  - c\n",
        "l:\n  - a\n  - y\n  - c\n",
    );
    assert_eq!(
        text,
        "l:\n  - a\n<<<<<<<\n  - x\n|||||||\n  - b\n=======\n  - y\n>>>>>>>\n  - c\n"
    );
    assert_eq!(paths, vec![path(&["l", "[1]"])]);
}

#[test]
fn list_item_removed_and_changed() {
    let (text, paths) = conflicted(
        "l:\n  - a\n  - b\n  - c\n",
        "l:\n  - a\n  - c\n",
        "l:\n  - a\n  - y\n  - c\n",
    );
    assert_eq!(
        text,
        "l:\n  - a\n<<<<<<<\n|||||||\n  - b\n=======\n  - y\n>>>>>>>\n  - c\n"
    );
    assert_eq!(paths, vec![path(&["l"])]);
}

#[test]
fn list_comments_preserved() {
    let base = "l:\n  # first\n  - a  # note\n  - b\n";
    let ours = "l:\n  # first\n  - a  # note\n  - b\n  - c  # ours\n";
    let theirs = "l:\n  # the first\n  - a  # note\n  - b\n";
    assert_eq!(
        clean(base, ours, theirs),
        "l:\n  # the first\n  - a  # note\n  - b\n  - c  # ours\n"
    );
}

#[test]
fn list_of_mappings_edits_to_different_fields() {
    let base = "containers:\n  - name: web\n    image: nginx:1\n    port: 80\n";
    let ours = "containers:\n  - name: web\n    image: nginx:2\n    port: 80\n";
    let theirs = "containers:\n  - name: web\n    image: nginx:1\n    port: 8080\n";
    assert_eq!(
        clean(base, ours, theirs),
        "containers:\n  - name: web\n    image: nginx:2\n    port: 8080\n"
    );
}

#[test]
fn list_of_mappings_first_key_changes() {
    let base = "l:\n  - a: 1\n    b: 2\n";
    let ours = "l:\n  - a: 10\n    b: 2\n";
    let theirs = "l:\n  - z: 0\n    a: 1\n    b: 20\n";
    assert_eq!(
        clean(base, ours, theirs),
        "l:\n  - z: 0\n    a: 10\n    b: 20\n"
    );
}

#[test]
fn list_of_mappings_same_field_conflicts() {
    let base = "l:\n  - name: web\n    image: a\n";
    let ours = "l:\n  - name: web\n    image: b\n";
    let theirs = "l:\n  - name: web\n    image: c\n";
    let (text, paths) = conflicted(base, ours, theirs);
    assert_eq!(
        text,
        "l:\n  - name: web\n<<<<<<<\n    image: b\n|||||||\n    image: a\n=======\n    image: c\n>>>>>>>\n"
    );
    assert_eq!(paths, vec![path(&["l", "[0]", "image"])]);
}

#[test]
fn list_added_on_both_sides() {
    assert_eq!(
        clean("a: 1\n", "a: 1\nl:\n  - x\n", "a: 1\nl:\n  - y\n"),
        "a: 1\nl:\n  - x\n  - y\n"
    );
}

#[test]
fn list_appended_item_is_reindented() {
    assert_eq!(
        clean("l:\n- a\n", "l:\n  - a\n", "l:\n- a\n- b:\n    c: 1\n"),
        "l:\n  - a\n  - b:\n      c: 1\n"
    );
}

#[test]
fn flow_sequence_merges_by_item() {
    assert_eq!(
        clean("l: [a, b]\n", "l: [x, b]\n", "l: [a, b, c]\n"),
        "l: [x, b, c]\n"
    );
}

#[test]
fn flow_sequence_conflict_covers_the_line() {
    let (text, paths) = conflicted(
        "l: [a, b]  # c\nz: 1\n",
        "l: [x, b]  # c\nz: 1\n",
        "l: [y, b]  # c\nz: 1\n",
    );
    assert_eq!(
        text,
        "<<<<<<<\nl: [x, b]  # c\n|||||||\nl: [a, b]  # c\n=======\nl: [y, b]  # c\n>>>>>>>\nz: 1\n"
    );
    assert_eq!(paths, vec![path(&["l"])]);
}

#[test]
fn theirs_added_entry_is_reindented() {
    let base = "s:\n  a: 1\n";
    let ours = "s:\n    a: 1\n";
    let theirs = "s:\n  a: 1\n  b:\n    c: 2\n";
    assert_eq!(
        clean(base, ours, theirs),
        "s:\n    a: 1\n    b:\n      c: 2\n"
    );
}

#[test]
fn reorder_by_theirs_is_followed() {
    assert_eq!(
        clean(
            "a: 1\nb: 2\nc: 3\n",
            "a: 10\nb: 2\nc: 3\n",
            "c: 3\nb: 2\na: 1\n"
        ),
        "c: 3\nb: 2\na: 10\n"
    );
}

#[test]
fn reorders_on_both_sides_conflict() {
    let (_, paths) = conflicted(
        "a: 1\nb: 2\nc: 3\n",
        "b: 2\na: 1\nc: 3\n",
        "a: 1\nc: 3\nb: 2\n",
    );
    assert_eq!(paths, vec![path(&[])]);
}

#[test]
fn missing_final_newline() {
    assert_eq!(clean("a: 1\n", "a: 1", "a: 1\nb: 2\n"), "a: 1\nb: 2\n");
    assert_eq!(
        clean("a: 1\nb: 2", "a: 1\nb: 3", "a: 2\nb: 2"),
        "a: 2\nb: 3"
    );
}

#[test]
fn multiple_documents() {
    let base = "a: 1\n---\nb: 1\nc: 1\n";
    let ours = "a: 1\n---\nb: 2\nc: 1\n";
    let theirs = "a: 1\n---\nb: 1\nc: 2\n";
    assert_eq!(clean(base, ours, theirs), "a: 1\n---\nb: 2\nc: 2\n");
}

#[test]
fn sequence_root() {
    assert_eq!(
        clean("- a\n", "- a\n- b\n", "- a\n- c\n"),
        "- a\n- b\n- c\n"
    );
    let (text, paths) = conflicted("- a\n", "- b\n", "- c\n");
    assert_eq!(text, "<<<<<<<\n- b\n|||||||\n- a\n=======\n- c\n>>>>>>>\n");
    assert_eq!(paths, vec![path(&["[0]"])]);
}

#[test]
fn scalar_root_is_atomic() {
    assert_eq!(clean("a\n", "a\n", "b\n"), "b\n");
    let (_, paths) = conflicted("a\n", "b\n", "c\n");
    assert_eq!(paths, vec![path(&[])]);
}

#[test]
fn flow_mapping_root_merges_by_key() {
    assert_eq!(
        clean("{a: 1, b: 2}\n", "{a: 2, b: 2}\n", "{a: 1, b: 3}\n"),
        "{a: 2, b: 3}\n"
    );
}

#[test]
fn flow_mapping_root_conflict() {
    let (text, paths) = conflicted("{a: 1}\n", "{a: 2}\n", "{a: 3}\n");
    assert_eq!(
        text,
        "<<<<<<<\n{a: 2}\n|||||||\n{a: 1}\n=======\n{a: 3}\n>>>>>>>\n"
    );
    assert_eq!(paths, vec![Conflict { path: vec![] }.path]);
}

#[test]
fn quoted_key_matches_plain_key() {
    assert_eq!(
        clean("a: 1\nb: 1\n", "\"a\": 1\nb: 1\n", "a: 1\nb: 2\n"),
        "\"a\": 1\nb: 2\n"
    );
}

#[test]
fn list_of_mappings_first_key_removed() {
    let base = "l:\n  - a: 1\n    b: 2\n";
    let ours = "l:\n  - a: 1\n    b: 3\n";
    let theirs = "l:\n  - b: 2\n";
    assert_eq!(clean(base, ours, theirs), "l:\n  - b: 3\n");
}

#[test]
fn list_of_mappings_conflict_on_first_key() {
    let (text, paths) = conflicted("- a: 1\n", "- a: 2\n", "- a: 3\n");
    assert_eq!(
        text,
        "<<<<<<<\n- a: 2\n|||||||\n- a: 1\n=======\n- a: 3\n>>>>>>>\n"
    );
    assert_eq!(paths, vec![path(&["[0]"])]);
}

#[test]
fn block_scalar_edits_on_different_lines() {
    let base = "script: |\n  one\n  two\n  three\n  four\nnext: 1\n";
    let ours = "script: |\n  ONE\n  two\n  three\n  four\nnext: 1\n";
    let theirs = "script: |\n  one\n  two\n  three\n  FOUR\nnext: 1\n";
    assert_eq!(
        clean(base, ours, theirs),
        "script: |\n  ONE\n  two\n  three\n  FOUR\nnext: 1\n"
    );
}

#[test]
fn block_scalar_conflict_is_narrowed_to_lines() {
    let base = "script: |\n  one\n  two\n  three\n";
    let ours = "script: |\n  one\n  TWO\n  three\n";
    let theirs = "script: |\n  one\n  2\n  three\n";
    let (text, paths) = conflicted(base, ours, theirs);
    assert_eq!(
        text,
        "script: |\n  one\n<<<<<<<\n  TWO\n|||||||\n  two\n=======\n  2\n>>>>>>>\n  three\n"
    );
    assert_eq!(paths, vec![path(&["script"])]);
}

#[test]
fn multi_line_flow_mapping_merges_by_key() {
    let base = "m: {\n  a: 1,\n  b: 2,\n  c: 3\n}\n";
    let ours = "m: {\n  a: 10,\n  b: 2,\n  c: 3\n}\n";
    let theirs = "m: {\n  a: 1,\n  b: 2,\n  c: 30\n}\n";
    assert_eq!(
        clean(base, ours, theirs),
        "m: {\n  a: 10,\n  b: 2,\n  c: 30\n}\n"
    );
}

#[test]
fn scalar_root_merges_by_line() {
    let base = "|\n  one\n  two\n  three\n";
    let ours = "|\n  ONE\n  two\n  three\n";
    let theirs = "|\n  one\n  two\n  THREE\n";
    assert_eq!(clean(base, ours, theirs), "|\n  ONE\n  two\n  THREE\n");
}

#[test]
fn list_with_aliases() {
    let base = "x: &a 1\nl:\n  - *a\n  - b\n";
    let ours = "x: &a 1\nl:\n  - *a\n  - b\n  - c\n";
    let theirs = "x: &a 1\nl:\n  - z\n  - *a\n  - b\n";
    assert_eq!(
        clean(base, ours, theirs),
        "x: &a 1\nl:\n  - z\n  - *a\n  - b\n  - c\n"
    );
}

#[test]
fn flow_mapping_merges_by_key() {
    assert_eq!(
        clean(
            "m: {a: 1, b: 2}\n",
            "m: {a: 10, b: 2}\n",
            "m: {a: 1, b: 20}\n"
        ),
        "m: {a: 10, b: 20}\n"
    );
}

#[test]
fn flow_mapping_additions_and_deletions() {
    assert_eq!(
        clean(
            "m: {a: 1, b: 2, c: 3}\n",
            "m: {a: 1, c: 3, d: 4}\n",
            "m: {a: 1, b: 2, c: 30}\n"
        ),
        "m: {a: 1, c: 30, d: 4}\n"
    );
}

#[test]
fn multi_line_flow_mapping_adjacent_changes() {
    let base = "m: {\n  a: 1,\n  b: 2\n}\n";
    let ours = "m: {\n  a: 1,\n  b: 2,\n  c: 3\n}\n";
    let theirs = "m: {\n  a: 1,\n  b: 20\n}\n";
    assert_eq!(
        clean(base, ours, theirs),
        "m: {\n  a: 1,\n  b: 20,\n  c: 3\n}\n"
    );
}

#[test]
fn json_root() {
    let base = "{\n  \"a\": 1,\n  \"b\": {\"x\": [1, 2]}\n}\n";
    let ours = "{\n  \"a\": 10,\n  \"b\": {\"x\": [1, 2]}\n}\n";
    let theirs = "{\n  \"a\": 1,\n  \"b\": {\"x\": [1, 2, 3]}\n}\n";
    assert_eq!(
        clean(base, ours, theirs),
        "{\n  \"a\": 10,\n  \"b\": {\"x\": [1, 2, 3]}\n}\n"
    );
}

#[test]
fn flow_mapping_conflict_is_not_merged_by_line() {
    let base = "m: {\n  a: 1,\n  b: 2\n}\n";
    let ours = "m: {\n  a: 10,\n  b: 2\n}\n";
    let theirs = "m: {\n  a: 11,\n  b: 2\n}\n";
    let (text, paths) = conflicted(base, ours, theirs);
    assert_eq!(
        text,
        "<<<<<<<\nm: {\n  a: 10,\n  b: 2\n}\n|||||||\nm: {\n  a: 1,\n  b: 2\n}\n=======\nm: {\n  a: 11,\n  b: 2\n}\n>>>>>>>\n"
    );
    assert_eq!(paths, vec![path(&["m"])]);
}

#[test]
fn tagged_mapping_merges_by_key() {
    assert_eq!(
        clean(
            "d: !t\n  a: 1\n  b: 2\n",
            "d: !t\n  a: 10\n  b: 2\n",
            "d: !t\n  a: 1\n  b: 20\n"
        ),
        "d: !t\n  a: 10\n  b: 20\n"
    );
}

#[test]
fn duplicate_keys_match_by_occurrence() {
    assert_eq!(
        clean(
            "a: 1\na: 2\nb: 1\nc: 1\n",
            "a: 1\na: 20\nb: 10\nc: 1\n",
            "a: 10\na: 2\nb: 1\nc: 10\n"
        ),
        "a: 10\na: 20\nb: 10\nc: 10\n"
    );
}

#[test]
fn complex_keys() {
    assert_eq!(
        clean(
            "? [x, y]\n: 1\nb: 1\nc: 1\n",
            "? [x, y]\n: 1\nb: 10\nc: 1\n",
            "? [x, y]\n: 2\nb: 1\nc: 10\n"
        ),
        "? [x, y]\n: 2\nb: 10\nc: 10\n"
    );
}

#[test]
fn key_line_comment_conflict_is_narrow() {
    let base = "s:  # c\n  a: 1\n  b: 2\n";
    let ours = "s:  # ours\n  a: 10\n  b: 2\n";
    let theirs = "s:  # theirs\n  a: 1\n  b: 20\n";
    let (text, paths) = conflicted(base, ours, theirs);
    assert_eq!(
        text,
        "<<<<<<<\ns:  # ours\n|||||||\ns:  # c\n=======\ns:  # theirs\n>>>>>>>\n  a: 10\n  b: 20\n"
    );
    assert_eq!(paths, vec![path(&["s"])]);
}

#[test]
fn changed_type_is_not_merged_by_line() {
    // Merged line by line, theirs' change to the nested mapping would end
    // up inside the string ours turned it into.
    let base = "k:\n  a: 1\n  b: 2\n  c: 3\nz: 1\n";
    let ours = "k: |\n  a: 1\n  b: 2\n  c: 3\nz: 1\n";
    let theirs = "k:\n  a: 1\n  b: 2\n  c: 4\nz: 1\n";
    let (text, paths) = conflicted(base, ours, theirs);
    assert_eq!(
        text,
        "<<<<<<<\nk: |\n  a: 1\n  b: 2\n  c: 3\n|||||||\nk:\n  a: 1\n  b: 2\n  c: 3\n=======\nk:\n  a: 1\n  b: 2\n  c: 4\n>>>>>>>\nz: 1\n"
    );
    assert_eq!(paths, vec![path(&["k"])]);
}

#[test]
fn keys_added_at_the_same_place_ours_first() {
    assert_eq!(
        clean("a: 1\nz: 1\n", "a: 1\nb: 1\nz: 1\n", "a: 1\nc: 1\nz: 1\n"),
        "a: 1\nb: 1\nc: 1\nz: 1\n"
    );
}

#[test]
fn keys_added_at_the_same_place_ours_first_in_theirs_order() {
    assert_eq!(
        clean(
            "a: 1\nb: 1\nz: 1\n",
            "a: 1\nx: 1\nb: 1\nz: 1\n",
            "b: 1\na: 1\ny: 1\nz: 1\n"
        ),
        "b: 1\na: 1\nx: 1\ny: 1\nz: 1\n"
    );
}

#[test]
fn list_item_comment_change_is_kept() {
    assert_eq!(
        clean(
            "images:\n  - nginx:1.25  # pinned\n",
            "images:\n  - nginx:1.27  # pinned\n",
            "images:\n  - nginx:1.25  # pinned until #42 is fixed\n"
        ),
        "images:\n  - nginx:1.27  # pinned until #42 is fixed\n"
    );
}

#[test]
fn list_item_reformatting_is_kept() {
    assert_eq!(
        clean(
            "l:\n  - a\n  - b\n",
            "l:\n  - a\n  - c\n",
            "l:\n  - 'a'\n  - b\n"
        ),
        "l:\n  - 'a'\n  - c\n"
    );
}

#[test]
fn flow_item_reformatting_is_kept() {
    assert_eq!(
        clean("l: [a, b]\n", "l: [a, c]\n", "l: ['a', b]\n"),
        "l: ['a', c]\n"
    );
}

#[test]
fn value_and_comment_on_the_same_line() {
    assert_eq!(
        clean(
            "replicas: 2  # one per zone\n",
            "replicas: 3  # one per zone\n",
            "replicas: 2  # one per zone, see RUNBOOK.md\n"
        ),
        "replicas: 3  # one per zone, see RUNBOOK.md\n"
    );
}

#[test]
fn key_and_value_on_the_same_line() {
    assert_eq!(
        clean("name: web\n", "\"name\": web\n", "name: api\n"),
        "\"name\": api\n"
    );
}

#[test]
fn respelled_value_and_comment_on_the_same_line() {
    assert_eq!(
        clean("a: x  # c\n", "a: 'x'  # c\n", "a: x  # d\n"),
        "a: 'x'  # d\n"
    );
}

#[test]
fn value_respelled_on_one_side_changed_on_the_other() {
    assert_eq!(
        clean("a: x  # c\n", "a: 'x'  # c\n", "a: y  # c\n"),
        "a: y  # c\n"
    );
}

#[test]
fn same_line_value_conflict() {
    let (text, paths) = conflicted("a: 1  # c\n", "a: 2  # c\n", "a: 3  # d\n");
    assert_eq!(
        text,
        "<<<<<<<\na: 2  # c\n|||||||\na: 1  # c\n=======\na: 3  # d\n>>>>>>>\n"
    );
    assert_eq!(paths, vec![path(&["a"])]);
}

#[test]
fn flow_entry_key_and_value() {
    assert_eq!(
        clean("{a: 1, b: 2}\n", "{\"a\": 1, b: 2}\n", "{a: 10, b: 2}\n"),
        "{\"a\": 10, b: 2}\n"
    );
}
