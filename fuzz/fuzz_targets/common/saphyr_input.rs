//! Which inputs saphyr can be used as an oracle for.

/// Whether `input` is one that saphyr 0.0.12 cannot be compared on:
/// it has C0 controls, which parsers handle inconsistently, a construct the
/// reference parsers disagree on, or a directive that hangs saphyr.
pub fn unsuitable(input: &str) -> bool {
    // C0 controls other than tab and newline are not valid YAML 1.2 and
    // parsers handle them inconsistently.
    if input.bytes().any(|b| b < 0x20 && b != b'\t' && b != b'\n') {
        return true;
    }

    // A `:` straight after an anchor or alias name is another grey area:
    // saphyr reads `k: &a: v` as `k: v` and drops the anchor, PyYAML
    // rejects it as "mapping values are not allowed here". With the
    // reference parsers split there is no answer to hold yaml-edit to.
    if input.lines().any(|line| {
        line.split_whitespace()
            .any(|word| word.starts_with(['&', '*']) && word.contains(':'))
    }) {
        return true;
    }

    // saphyr 0.0.12 hangs forever on a reserved directive: a `%` starting
    // any line, followed by a name other than YAML or TAG (`%FOO`, `%!!`).
    // It need not be the first line -- `%TAG ! tag:e\n%!! v` wedges it too.
    // Reported upstream; skip so the oracle cannot hang the run. yaml-edit
    // parses these in microseconds.
    // The name has to be delimited to count as YAML or TAG: `%TAG!!x` is a
    // reserved directive named `TAG!!x`, and hangs saphyr just as `%FOO`
    // does.
    input.lines().any(|line| {
        let Some(rest) = line.strip_prefix('%') else {
            return false;
        };
        // YAML delimits a directive name with a space or tab, not with
        // whatever char::is_whitespace admits: `%TAG\u{a0}x` names the
        // reserved directive `TAG\u{a0}x`, and wedges saphyr as `%FOO` does.
        let name = rest.split([' ', '\t']).next().unwrap_or(rest);
        if name != "YAML" && name != "TAG" {
            return true;
        }
        // Even a well-named directive wedges saphyr when nothing follows it
        // on the line: `%TAG` alone asks for a 2GB allocation and aborts, as
        // `%YAML` does. Anything after the name avoids it.
        rest[name.len()..].trim_matches([' ', '\t']).is_empty()
    })
}
