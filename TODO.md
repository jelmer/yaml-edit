# TODO: yaml-edit

### Code Quality

**Reduce nesting in mutation methods**
- The worst offenders are done: `set_with_field_order` (9 levels to 6),
  `rename_key` (8 to 6) and `Sequence::remove` (8 to 5).
- `reorder_fields`, `insert_at_index_preserving` and `Sequence::set` are
  still around 7 levels and 100 lines each.

**Evaluate YamlValue necessity**
- YamlValue is a detached representation that loses formatting
- May be able to simplify by using AsYaml trait everywhere
- Consider removal if not serving a clear purpose

### Developer Experience

**YAML 1.1 compatibility warnings**
- Detect `yes/no`, octal `0755`, etc.
- Migration helpers for YAML 1.1 → 1.2 conversion

**Optional serde integration**
- Support struct serialization/deserialization

**Consistent formatting tool**
- Pretty-printer for standardizing YAML style

### Testing & Validation

Both items here are covered: `tests/invariants.rs` and
`tests/proptest_invariants.rs` hold round-trip and format-preservation
properties over generated mutation sequences, and `tests/yaml_test_suite.rs`
prints a pass/fail conformance report, with per-case detail under
`VERBOSE=1`.

### Known deviations from other parsers

These were each checked against the YAML 1.2 spec and both reference
parsers, and left as they are. Listed so they are not re-investigated.

Compare against `yaml.parse`, not `yaml.safe_load`: the loader raises on
unhashable and duplicate keys, which looks like a parse failure but says
nothing about how the input parses. saphyr's `load` deduplicates equal
keys for the same reason.

**An explicit key's value indicator indented past the mapping**
- `?\n  : c\n` reads as `{null: c}` here.
- `c-l-block-map-explicit-value(n)` wants `s-indent(n)` before the `:`, so
  strictly the colon must sit at the mapping's own column and PyYAML
  rejects the input. saphyr accepts this one spelling but rejects the
  equivalent `? a\n  : c\n` and `?\n  x\n  : c\n`.
- Accepting it uniformly is the lenient reading, and the only one of the
  three that treats all four spellings alike.

**`!!: v`**
- Read as the tag `!!:` on the scalar `v`, matching PyYAML. saphyr reports
  the document as bad.

**Repeated empty keys**
- `- :\n  :\n` and `- ?\n  ?\n` give two entries with equal null keys.
  saphyr's parser emits the same four scalars; only its loader collapses
  them, which is constructor behaviour rather than a parse difference.
  The suite's 2JQS expects the same two entries.
