# TODO: yaml-edit

### Code Quality

**Reduce nesting in mutation methods**
- nodes/mapping.rs and nodes/sequence.rs have deeply nested code
- Extract helper functions to improve readability

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

**Property-based testing**
- Round-trip invariants
- Format preservation properties

**YAML spec conformance**
- Automated conformance report generator


### Known deviations from other parsers

These were each checked against the YAML 1.2 spec and both reference
parsers, and left as they are. Listed so they are not re-investigated.

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
