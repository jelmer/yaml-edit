//! Check that the examples in README.md show what the merge actually does.
//!
//! Each example has a diff for ours, a diff for theirs and the merged
//! result, in that order. The base and the two sides are reconstructed from
//! the diffs.

use yaml_merge3::{merge, MergeOptions};

/// The fenced code blocks in `text`, as (info string, contents).
fn code_blocks(text: &str) -> Vec<(String, String)> {
    let mut blocks = Vec::new();
    let mut current: Option<(String, String)> = None;
    for line in text.lines() {
        match (&mut current, line.strip_prefix("```")) {
            (None, Some(info)) => current = Some((info.to_string(), String::new())),
            (Some(_), Some(_)) => blocks.push(current.take().unwrap()),
            (Some((_, contents)), None) => {
                contents.push_str(line);
                contents.push('\n');
            }
            (None, None) => {}
        }
    }
    assert!(current.is_none(), "unterminated code block");
    blocks
}

/// The old and new text of a full-context diff.
fn apply(diff: &str) -> (String, String) {
    let (mut old, mut new) = (String::new(), String::new());
    for line in diff.lines() {
        let (marker, rest) = match line.char_indices().nth(1) {
            Some((i, _)) => line.split_at(i),
            None => (line, ""),
        };
        match marker {
            " " | "" => {
                old.push_str(rest);
                old.push('\n');
                new.push_str(rest);
                new.push('\n');
            }
            "-" => {
                old.push_str(rest);
                old.push('\n');
            }
            "+" => {
                new.push_str(rest);
                new.push('\n');
            }
            _ => panic!("unexpected diff line: {line:?}"),
        }
    }
    (old, new)
}

#[test]
fn readme_examples() {
    let readme =
        std::fs::read_to_string(concat!(env!("CARGO_MANIFEST_DIR"), "/README.md")).unwrap();
    let examples = readme
        .split("## Examples")
        .nth(1)
        .unwrap()
        .split("\n## ")
        .next()
        .unwrap();
    let options = MergeOptions {
        ours_label: Some("ours".into()),
        base_label: Some("base".into()),
        theirs_label: Some("theirs".into()),
        ..Default::default()
    };

    let mut checked = 0;
    for section in examples.split("\n### ").skip(1) {
        let title = section.lines().next().unwrap();
        let blocks = code_blocks(section);
        let [(ours_info, ours_diff), (theirs_info, theirs_diff), (_, expected), ..] = &blocks[..]
        else {
            panic!("example {title:?} needs two diffs and a result");
        };
        assert_eq!(
            (ours_info.as_str(), theirs_info.as_str()),
            ("diff", "diff"),
            "{title}"
        );
        let (base, ours) = apply(ours_diff);
        let (theirs_base, theirs) = apply(theirs_diff);
        assert_eq!(
            base, theirs_base,
            "the diffs in {title:?} have different bases"
        );

        let result = merge(&base, &ours, &theirs, &options);
        assert_eq!(&result.text, expected, "{title}");
        checked += 1;
    }
    assert_eq!(checked, 6);
}
