# Differences from other YAML parsers

yaml-edit follows the YAML 1.2 spec and is checked against its test suite.
On a few inputs it reads YAML differently from PyYAML or saphyr. Each of
these was checked against the spec and both parsers and left as it is; they
are listed here so they are not re-investigated. `tests/compatibility.rs`
pins each of them.

Where PyYAML is concerned, compare against `yaml.parse` rather than
`yaml.safe_load`: the loader raises on unhashable and duplicate keys, which
looks like a parse failure but says nothing about how the input parses.
saphyr's `load` deduplicates equal keys for the same reason.

## An explicit key's value indicator indented past the mapping

`?\n  : c\n` reads as `{null: c}`, and likewise `? a\n  : c\n` and
`?\n  x\n  : c\n` read as `{a: c}` and `{x: c}`.

`c-l-block-map-explicit-value(n)` wants `s-indent(n)` before the `:`, so
strictly the colon has to sit at the mapping's own column, and PyYAML
rejects all three. saphyr accepts the first spelling but rejects the other
two. The parser accepts them alike, which is the lenient reading and the
only one of the three that treats all of these spellings the same way; the
validator reports the misplaced `:`.

## `!!: v`

Read as the tag `!!:` on the scalar `v`, as PyYAML does. saphyr reports the
document as bad.

## Repeated empty keys

`- :\n  :\n` and `- ?\n  ?\n` give two entries with equal null keys.
saphyr's parser emits the same events; only its loader collapses them, which
is constructor behaviour rather than a difference in parsing. The suite's
2JQS expects the same two entries. The validator reports the duplicate key,
as it does for any other.
