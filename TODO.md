# TODO: yaml-edit

### Code Quality

**Replace `.contains()` with `assert_eq!()` in tests**
- ~17 uses in src/yaml.rs, src/nodes/document.rs
- 1 use in tests/yaml_spec_compliance.rs
- Tests should verify exact output, not substring matching

**Reduce nesting in mutation methods**
- nodes/mapping.rs and nodes/sequence.rs have deeply nested code
- Extract helper functions to improve readability

**Evaluate YamlValue necessity**
- YamlValue is a detached representation that loses formatting
- May be able to simplify by using AsYaml trait everywhere
- Consider removal if not serving a clear purpose

### Features

**Complete semantic tagged collection support**
- Set (`!!set`) - basic support exists, needs enhancement
- Ordered mapping (`!!omap`) - needs dedicated type with ordering guarantees
- Pairs (`!!pairs`) - needs type allowing duplicate keys
- Binary data (`!!binary`) - needs base64 decoding/encoding
- Timestamp (`!!timestamp`) - needs ISO 8601 parsing

**Alias dereferencing API**
- Optional `resolve_aliases()` method for semantic expansion
- Handle circular references gracefully
- Keep lossless mode as default

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

**Fuzzing**
- Edge cases and malformed input

**Performance tracking**
- Regression tests
- Benchmark suite

**YAML spec conformance**
- Automated conformance report generator

### Advanced Features

**Merge key expansion**
- `<<: *anchor` resolution
