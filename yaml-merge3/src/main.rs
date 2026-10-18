//! Command line interface, following `git merge-file`.

use clap::Parser;
use std::path::PathBuf;
use std::process::ExitCode;
use yaml_merge3::{merge, MergeOptions};

/// Merge the changes from <BASE> to <OTHER> into <CURRENT>.
///
/// Exits with the number of conflicts, or 255 on error.
#[derive(Parser)]
#[command(version)]
struct Args {
    /// Write the result to stdout instead of overwriting <CURRENT>.
    #[arg(short = 'p', long)]
    stdout: bool,

    /// Do not report conflicts on stderr.
    #[arg(short, long)]
    quiet: bool,

    /// Labels for the current, base and other versions in conflict markers.
    /// They default to the file names.
    #[arg(short = 'L', value_name = "LABEL", num_args = 1, action = clap::ArgAction::Append)]
    labels: Vec<String>,

    /// Number of characters in conflict markers.
    #[arg(long, default_value_t = 7)]
    marker_size: usize,

    /// Our version, which receives the result.
    current: PathBuf,
    /// The common ancestor.
    base: PathBuf,
    /// Their version.
    other: PathBuf,
}

/// Spell a conflict path as `a.b[0].c`.
fn format_path(path: &[String]) -> String {
    let mut out = String::new();
    for element in path {
        if !out.is_empty() && !element.starts_with('[') {
            out.push('.');
        }
        out.push_str(element);
    }
    out
}

fn run(args: Args) -> Result<usize, String> {
    if args.labels.len() > 3 {
        return Err("too many labels".to_string());
    }
    let mut labels = args.labels.into_iter();
    let mut label =
        |path: &PathBuf| Some(labels.next().unwrap_or_else(|| path.display().to_string()));
    let options = MergeOptions {
        ours_label: label(&args.current),
        base_label: label(&args.base),
        theirs_label: label(&args.other),
        marker_size: args.marker_size,
    };

    let read = |path: &PathBuf| {
        std::fs::read_to_string(path).map_err(|e| format!("{}: {e}", path.display()))
    };
    let result = merge(
        &read(&args.base)?,
        &read(&args.current)?,
        &read(&args.other)?,
        &options,
    );

    if args.stdout {
        print!("{}", result.text);
    } else {
        std::fs::write(&args.current, &result.text)
            .map_err(|e| format!("{}: {e}", args.current.display()))?;
    }
    if !args.quiet {
        for conflict in &result.conflicts {
            match conflict.path.as_slice() {
                [] => eprintln!("conflict at document level"),
                path => eprintln!("conflict in {}", format_path(path)),
            }
        }
    }
    Ok(result.conflicts.len())
}

fn main() -> ExitCode {
    match run(Args::parse()) {
        // Like git merge-file, exit with the number of conflicts.
        Ok(conflicts) => ExitCode::from(conflicts.min(127) as u8),
        Err(e) => {
            eprintln!("yaml-merge3: {e}");
            ExitCode::from(255)
        }
    }
}
