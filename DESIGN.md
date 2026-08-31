# Design

This document covers the architecture and key patterns in `yaml-edit`.

## Lossless editing

The primary goal is lossless editing - modifying YAML files while preserving
formatting, comments, whitespace, quote styles, key ordering, and
anchors/aliases. This distinguishes `yaml-edit` from libraries that parse into
a data model and re-serialize, losing formatting.

## Rowan foundation

The crate is built on [rowan](https://github.com/rust-analyzer/rowan), the
lossless syntax tree library from rust-analyzer. Rowan provides a two-layer
tree: immutable, deduplicated "green" nodes for storage, and mutable "red"
nodes (`SyntaxNode`) for navigation and mutation. Red nodes use interior
mutability, so methods take `&self` rather than `&mut self`.

This is documented in the crate-level docs with examples, since it's
surprising for Rust developers.

## Type hierarchy

The syntax tree types (`Document`, `Mapping`, `Sequence`, `Scalar`,
`TaggedNode`) are thin wrappers around `SyntaxNode`. A `YamlFile` contains one
or more `Document`s, each of which holds a tree of these wrappers.
`MappingEntry` is the key-value pair inside a `Mapping`; `Directive` covers
`%YAML` and `%TAG` headers.

`YamlNode` is a type-erased enum (`Scalar | Mapping | Sequence | TaggedNode |
Alias`) returned by navigation methods like `Mapping::get()`.

## `splice_children` for mutation

Never rebuild entire nodes. Use rowan's `splice_children` to replace only the
parts that changed. This preserves formatting on everything else.

When using `splice_children`, collect the new children into a `Vec` first -
passing an iterator directly causes borrow conflicts:

```rust
let children: Vec<_> = new_node.children_with_tokens()
    .map(|c| c.into())
    .collect();
self.0.splice_children(range, children);
```

Also note that `splice_children` uses `children_with_tokens()` indices, not
`children()` indices.

## `AsYaml` trait

Mutation APIs accept `impl AsYaml` rather than concrete types. This lets
syntax nodes pass through without serializing to a string and re-parsing,
preserving formatting and comments. Primitive types (`i64`, `&str`, `bool`,
etc.) also implement `AsYaml` for ergonomic use.

The trait:

```rust
pub trait AsYaml {
    fn as_node(&self) -> Option<&SyntaxNode>;
    fn kind(&self) -> YamlKind;
    fn build_content(
        &self,
        builder: &mut rowan::GreenNodeBuilder,
        indent: usize,
        flow_context: bool,
    ) -> bool;  // returns true if content ends with NEWLINE
    fn is_inline(&self) -> bool;
}
```

Syntax wrappers return `Some` from `as_node()` and copy their existing tree
structure in `build_content()`. Primitive types return `None` and emit fresh
tokens. `is_inline()` controls whether the value goes on the same line as the
key (scalars, empties) or on a new indented line (block collections).

## `ScalarValue`: `string()` vs `parse()`

Two factory methods with explicit intent:

```rust
ScalarValue::string("123")  // always a string, no type detection
ScalarValue::parse("123")   // detects type -> integer
```

## Newline ownership

The full set of CST structural invariants lives in the module-level doc
comment at the top of `src/nodes/mod.rs` -- newline placement, INDENT
placement, entry termination, flow separators, and the rule against
whole-node rebuilds. That file is the authoritative spec for anyone
editing mutation code; the summary here is a quick reference.

YAML newlines are terminators, not separators. Every block-style
`MAPPING_ENTRY` and `SEQUENCE_ENTRY` owns its trailing `NEWLINE` token as a
direct child. The parent `MAPPING`/`SEQUENCE` node does not own any newlines
itself.

```
MAPPING_ENTRY
  KEY -> SCALAR "host"
  COLON
  WHITESPACE
  VALUE -> SCALAR "localhost"
  NEWLINE                      <- owned by the entry
```

When inserting or replacing entries, check whether the entry already ends with
a newline (nested collections do, scalars don't) before adding separators.

## Implicit-null values

Every `MAPPING_ENTRY` and `SEQUENCE_ENTRY` in the CST holds a value node,
even when the source omits it. The parser emits a zero-width
`SCALAR { NULL "" }` in that position for every "missing value" syntax:

- Block mapping value: `key:\n` or `key:` at EOF
- Flow mapping value: `{key}`, `{key:}`, `{key,}`, `{a:, b:}`
- Flow mapping key: `{,}`, `{: v}`, `{:}`
- Explicit-key mapping: `? key\n`, `? \n: v`, `? key\n:\n`
- Block sequence entry: `- \n`, `-\n`
- Flow sequence entry: `[a, , c]`, `[,]`, `[,,]`

Every KEY and VALUE always contains exactly one scalar or collection
node. Mappings wrap the value in a `VALUE` node; sequences hold the
scalar directly as a `SEQUENCE_ENTRY` child, matching how each shape
already treats non-null values. The zero-width `NULL ""` token renders
as nothing, so round-trips are lossless.

**Parsed nulls vs written nulls.** Programmatically-constructed nulls
(`YamlValue::Scalar(ScalarValue::null())`) build `SCALAR { NULL "null" }`
-- textual, three characters. On read, keep the parser's zero-width form
so unchanged nulls stay implicit; use the textual form only when writing
a new null.

**Consequences for accessors and mutators:**

- `len` / `get` / `set` / `remove` / `iter` on sequences and mappings all
  index by entry, not by "entry with a non-null value" -- so indexes agree
  across accessors and mutators.
- `Scalar::is_null()` returns `true` for the zero-width form.
- When replacing a zero-width null with a real value via `set`, insert a
  `WHITESPACE " "` between the preceding structural token (`DASH` in
  block sequences) and the new value -- the original had no separator
  because the value itself was zero-width. See `Sequence::set` for the
  pattern.

### Inline vs block values

After a colon in a mapping:
- **Inline** (WHITESPACE after colon): scalars, flow collections, empties
- **Block** (NEWLINE + INDENT after colon): non-empty block collections

After a dash in a sequence, content is always inline. For block
mappings/sequences as sequence items, the first entry shares the dash line;
subsequent entries go on new indented lines. This is the "inline-start"
pattern, as opposed to the "block-start" pattern used after colons.

### CST example

```yaml
config:
  host: localhost
  port: 8080
```

```
DOCUMENT
  MAPPING
    MAPPING_ENTRY
      KEY -> SCALAR "config"
      COLON
      VALUE
        NEWLINE
        INDENT "  "
        MAPPING
          MAPPING_ENTRY
            KEY -> SCALAR "host"
            COLON
            WHITESPACE
            VALUE -> SCALAR "localhost"
            NEWLINE
          INDENT "  "
          MAPPING_ENTRY
            KEY -> SCALAR "port"
            COLON
            WHITESPACE
            VALUE -> SCALAR "8080"
            NEWLINE
      NEWLINE
```

## Error handling

The primary error type is `YamlError` (in `src/error.rs`), with variants for
I/O, parse errors (with optional line/column), key-not-found (with available
keys), type mismatch, invalid index, and invalid operation.

Domain-specific error types (`CustomTagError`, `ValidationError`, etc.) are
kept separate intentionally.

## Code organization

- `lib.rs` - public API re-exports
- `yaml.rs` - core types, `YamlFile`, parser
- `lex.rs` - lexer, token types (`SyntaxKind`)
- `parse.rs` - parse result types
- `nodes/` - AST node wrappers (`Document`, `Mapping`, `Sequence`, …)
- `as_yaml.rs` - `AsYaml` trait, `YamlNode` enum, tagged collection types
- `value.rs` - `YamlValue` (deprecated)
- `scalar.rs` - `ScalarValue` and type detection
- `builder.rs` - fluent builder API
- `path.rs` - dot-separated path access
- `error.rs` - error types
- `schema.rs` - schema validation (Failsafe, JSON, Core)
- `custom_tags.rs` - custom tag registry
- `visitor.rs` - visitor pattern traversal
- `anchor_resolution.rs` - anchor/alias resolution
- `error_recovery.rs` - parse error recovery
- `validator.rs` - YAML spec validation
- `debug.rs` - tree visualization

## Checklist

- Read the CST-invariants doc at the top of `src/nodes/mod.rs` before
  editing mutation code
- Use `splice_children` for mutations - don't rebuild nodes
- Methods take `&self`, not `&mut self` (interior mutability)
- Test that formatting is preserved (lossless round-trip)
- Call `debug::validate_tree` + `debug::roundtrip_ok` (or the
  `assert_cst_ok` / `assert_file_cst_ok` test helpers) after every
  mutation in new tests
- Use `debug::print_tree()` to understand the CST structure
- Use `assert_eq!` with exact expected values in tests

## References

- [rowan](https://github.com/rust-analyzer/rowan)
- [YAML 1.2 Spec](https://yaml.org/spec/1.2.2/)
