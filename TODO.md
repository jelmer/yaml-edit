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

