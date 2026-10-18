# Design

This document covers the architecture and key patterns in `yaml-edit`.

## Principles

The primary goal is lossless editing: modifying YAML files while preserving
formatting, comments, whitespace, quote styles, key ordering, tags and
anchors/aliases. This distinguishes `yaml-edit` from libraries that parse into
a data model and re-serialize, losing formatting.

A few rules follow from that, and take precedence over convenience:

- **Byte-identical round trips.** Loading a file and writing it back without
  edits must reproduce it byte for byte. Nothing is rewritten implicitly; an
  edit only touches the text it was asked to change. Formatting information
  such as tags and quoting styles is carried through value types rather than
  dropped.
- **Follow the YAML 1.2 spec.** Typing follows the core schema, keys compare
  by tag and canonical value, and implicit keys obey the spec's length and
  line limits. The crate is tested against the YAML test suite.
- **The parser is lenient, the validator is strict.** The parser accepts
  anything it can turn into a sensible tree, so that slightly broken files can
  still be loaded and fixed. Spec violations are reported by the validator,
  not by refusing to parse. Handlers such as the JSON deserializer follow the
  same split.
- **Build trees, not text.** Builders and mutations construct CST nodes
  directly, in exactly the shape the parser would produce for the same text.
  They never render a string and re-parse it.
- **Never drop content or errors silently.** If an operation cannot be done
  without losing something (comments, entries, a tag), it reports that rather
  than quietly doing something else. A failed operation leaves the document
  unchanged.

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

`MergedMapping` is a read-only view of a mapping with aliases and `<<` merge
keys resolved. It and `Mapping` both implement `MappingView`, so read-only code
can work against either.

## Parsing and validation

The parser (`parser/`) produces a tree for anything it can represent: a
document with inconsistent indentation, a plain scalar starting with an
indicator, or a tab used for indentation still parses, and the tree
round-trips byte-identically. Parse errors are reserved for input that cannot
be turned into a sensible tree.

`Validator` walks the tree and reports spec violations with positions. A new
spec check goes into the validator, with a test, and not into the parser. The
YAML test suite harness counts an invalid case as handled when either the
parser or the validator rejects it, and requires the validator to accept every
valid case. Valid cases the validator still rejects
are listed in `KNOWN_VALIDATOR_FALSE_POSITIVES`; a listed case that starts
validating cleanly fails the run, so the list can only shrink.

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

## Builders

`MappingBuilder`, `SequenceBuilder` and `YamlBuilder` emit green nodes with
the same structure the parser produces: `MAPPING_ENTRY` with `KEY`, `COLON`
and `VALUE`, `SEQUENCE_ENTRY` with `DASH`, entries owning their trailing
`NEWLINE`, and so on. A built document must be indistinguishable from parsing
its own text: `len`, `get`, `set` and the other accessors behave the same,
and `debug::tree_to_string` gives the same output for both. String keys and
values are quoted where a plain scalar would be read back as something else.

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

`build_content` must respect `flow_context`: inside a flow collection, block
content has to be emitted in flow form, since block syntax is not allowed
there.

Strings written into a flow collection follow the quoting of the string
scalars already in it. If its keys and items that are strings are all
double-quoted, as in a JSON file, new strings are double-quoted too, so JSON
stays JSON. Otherwise they are quoted only where a plain scalar would not
read back as the same string, for instance when it holds a flow indicator
such as `,`. A collection with no strings of its own (empty, or only
numbers) goes by the nearest enclosing flow collection that has some, and
failing that quotes only where needed; a block collection ends the search.
This applies to keys and values of new and replaced entries, renamed keys,
and the contents of raw collections (`YamlValue`, `Vec`, ...) written there.
Numbers, booleans and nulls are unaffected, as are strings given an
explicit style and scalar nodes, which keep the style they were written in.
The choice is passed down as `FlowQuoting` through the hidden
`AsYaml::build_flow_content`.

## `ScalarValue` and scalar typing

Constructors with explicit intent:

```rust
ScalarValue::string("123")  // always a string, no type detection
ScalarValue::parse("123")   // detects type -> integer
```

`ScalarValue::from_scalar` reads a parsed scalar and respects its style and
tag: a quoted or block scalar is a string, whatever its text.

`ScalarValue::classify_plain` (returning `CoreScalarType`) is the single
implementation of YAML 1.2 core schema typing for plain scalars. The lexer and
`ScalarValue` both use it, so the two cannot disagree. `parse()` additionally
recognises timestamps, which are outside the core schema.

`migrate::to_yaml_1_2` rewrites plain scalars whose meaning changed between
YAML 1.1 and 1.2 so that a 1.2 reader sees the value a 1.1 reader saw. It is
explicit and opt-in, the only place where the crate changes the spelling of
existing content.

## Newline ownership

The full set of CST structural invariants lives in the module-level doc
comment of the `nodes` module -- newline placement, INDENT placement, entry
termination, flow separators, and the rule against whole-node rebuilds. That
comment is the authoritative spec for anyone editing mutation code; the
summary here is a quick reference.

YAML newlines are terminators, not separators. Every block-style
`MAPPING_ENTRY` and `SEQUENCE_ENTRY` owns its trailing `NEWLINE` token as a
direct child. The parent `MAPPING`/`SEQUENCE` node does not own any newlines
itself, except for blank lines and comment lines between entries.

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

Trivia outside the root collection lives at the document level: a leading BOM
and blank lines before the root, and the newline and blank lines after a
scalar or flow root, are children of `DOCUMENT`. Comments and directives
before the first document are children of the file root instead. Mutations
that add documents or directives must keep a BOM first in the stream.

A kept (`|+`, `>+`) block scalar takes every blank line after it into its
value. A new entry whose value ends in one therefore goes after any blank
lines at the place it is inserted rather than before them:
`insert_after("a", "k", "text\n\n")` on `a: 1\n\nb: 2\n` gives
`a: 1\n\nk: |+\n  text\n\nb: 2\n`. The text around the new entry stays as
it was, and the value keeps its style.

Any block scalar written by an edit, inserted or replacing a value, reads
the lines after it as body until one is indented less than its body. A
whitespace-only line with spaces past the body's column, or a tab, would
add text to its value, so that line loses its white space: `a: 1\n     \n`
with `k: "text\n"` inserted after `a` gives `a: 1\nk: |\n  text\n\n`. A
comment line indented as far as the body, after any blank lines, moves
back to the entry's column. Other lines are left as they are.

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
- Tag without content: `- !e\n` gives `TAGGED_NODE { TAG, SCALAR { NULL "" } }`

Every KEY and VALUE always contains exactly one scalar or collection
node. Mappings wrap the value in a `VALUE` node; sequences hold the
scalar directly as a `SEQUENCE_ENTRY` child, matching how each shape
already treats non-null values. The zero-width `NULL ""` token renders
as nothing, so round-trips are lossless.

**Parsed nulls vs written nulls.** Programmatically-constructed nulls
(`YamlValue::Scalar(ScalarValue::null())`) build `SCALAR { NULL "null" }`
-- textual, four characters. On read, keep the parser's zero-width form
so unchanged nulls stay implicit; use the textual form only when writing
a new null. When copying a collection into a form where an empty value is
not allowed (such as a block sequence written out as a flow sequence),
write the textual form so the entry is not lost.

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

## Comments around a replaced value

Replacing a value (`Mapping::set` on an existing key, `Sequence::set`,
`set_path`) keeps or removes the comments around the old value by
position, the same for mappings and sequences. A comment on the entry's
first line, the key's line after the colon or a sequence item's dash
line, belongs to the entry and stays, even when the old value started
on that line: `a: [1,  # c\n  2]`, `- x: 1  # c` and `- - 1  # c` set
to `5` give `a: 5  # c` and `- 5  # c`. It moves after the new value, or
onto the line where a new block value starts (`-  # c\n  y: 2`).
Comments on the old value's later lines go with it:
`a:  # c\n  # d\n  x: 1\n` with `a` set to `5` gives `a: 5  # c\n`.
Comments, blank lines and indentation outside the old value are kept as
they are, except that a comment line that would read as part of a new
block scalar's body is moved back to the entry's column. The blank lines
after a new block scalar are part of its body, as the parser reads them;
spaces on them that would be body text are dropped. A kept (`|+`) block
scalar takes every blank line after it into its value, and only a
comment line could end it before them, so those blank lines are dropped.

A block scalar whose header line has a comment of its own (`|  # src`)
leaves no room on that line for the entry's comment, which then goes on
a line of its own directly above the entry, at the entry's column.
An entry that is not in a mapping (one returned by `Mapping::remove`, say)
has no lines around it, so that line goes first in the entry, and the
lines that would follow it go at its end.

Moving an entry (`reorder_fields`, and `move_after`/`move_before` given
the entry's own value) takes along the comments on its lines and the
comment lines after it, up to the next entry, with any blank lines
among them: `a: 1  # c\n# d\nb: 2\n` reordered to `b, a` gives
`b: 2\na: 1  # c\n# d\n`. The blank lines after those belong to the
position, so `a: 1\n\nb: 2\n` reordered gives `b: 2\n\na: 1\n`. A block
scalar takes the blank lines after it into its value, so those of a
position whose entry now ends in one go after the next entry that does
not, or failing that the one before. Lines before the first entry stay
where they are. In a flow mapping the commas stay with their positions, and one
that has to follow an entry whose comment ends its line goes before
that comment. So does the indentation of the line after such a
comment: `{a: 1, # c\n b: 2 # d\n}` reordered gives
`{b: 2, # d\n a: 1 # c\n}`.

## Paths, anchors and aliases

Paths look like `server.hosts[0].name`, with `\` escapes for keys that
contain `.` or `[`. Lookups fall back to non-string keys (`1`, `true`) when no
string key matches. Malformed paths are a `PathParseError`, not a best
guess.

Reads resolve aliases. Writes through an alias are refused by default,
because they would change every other alias of the same anchor;
`AliasPolicy::Follow` writes to the anchored node and `AliasPolicy::Expand`
replaces the alias with an independent copy, built as CST.

Replacing a value (`Mapping::set`, `Sequence::set`, `set_path`,
`insert_at_index_preserving`) keeps the old value's anchor on the new one,
so aliases to it still resolve. A new value that is a node with an anchor
of its own keeps it only if the old value had none: the document's aliases
refer to the old anchor, and the node the value was copied from keeps its
own. A node passed as a new key keeps its anchor too.

Other anchors in a node that is removed or replaced (`remove`, `clear`,
`set`, `set_path`, `remove_path`, `rename_key`, ...) go with it, and so
would what the aliases after it refer to. Instead, the first alias that
referred to such an anchor is replaced with a copy of the anchored node,
anchor included, and the aliases after it refer to that copy:
`b: {c: &y 1}\nd: *y\n` with `b` set to `6` gives `b: 6\nd: &y 1\n`. The
same happens to the old anchor when the new value is an alias, which
cannot carry one. Only aliases that referred to the removed node count, so
`*y` in `a: &y 0\nb: {c: &y 1}\nd: *y\n` gets the copy rather than falling
back to `a`. Anchors within the copy that would take over aliases
referring to another node are left off it. An alias within the copy that
would then refer to the wrong node is in turn replaced with a copy, which
keeps only the anchors that aliases in it use. The shared helper for all
of this is `nodes::kept_aliases::keeping_aliases`.

An alias with a tag or anchor of its own, which the parser tolerates though
YAML does not, passes them on to its copy, which then goes without the
anchor it was copied with: a tag would change what later aliases to that
anchor refer to, and a node has room for one anchor only. Later aliases get
copies of their own. `k: !t *a` gives `k: !t 1` and `k: &b *a` gives
`k: &b 1`. The tag of the alias, being what was written at its place, also
takes the place of any tag the node has: with `a: &a !u 1`, `k: !t *a`
gives `k: !t 1`.

Moving entries (`reorder_fields`, and `move_after`/`move_before` given the
entry's own value) keeps every alias resolving to the node it did. An alias
that ends up before that node becomes a copy of it, anchor included, and
the node becomes an alias to the copy: `a: &x 1\nb: *x\n` reordered to
`b, a` gives `b: &x 1\na: *x\n`. The node stays as it is, a copy with its
own anchor, if it is a key, holds other anchors, or another anchor with
its name comes between. An alias that ends up after another anchor with
its name becomes a copy, with the anchor only if later aliases need it.
`move_after`/`move_before` given another value remove the old entry as
`remove` does. Sequences have no reordering methods.

An alias resolves to the closest preceding definition of its anchor in the
same document, as the spec requires. In `<<` merge keys only alias nodes are
merge operands; a quoted `'*a'` is a string.

Resolving aliases (`get_resolved`, `YamlValue::from_document`) reads an
alias with a tag of its own as its copy would read: `k: !t *a` as
`k: !t 1`. The tag of the alias also takes the place of any tag the node
has: with `a: &a !u 1`, `k: !t *a` reads as `k: !t 1`. An anchor on an
alias names the node the alias refers to, so `*b` in `b: &b *a\nc: *b\n`
resolves, on reads and through paths, to what `*a` does.

Mapping keys are compared everywhere by `as_yaml::NodeIdentity`: the
resolved tag and canonical content of the key, with aliases followed where
there is a registry to follow them. A `YamlValue` mapping holds each key by
the string form of its identity, so `? [a,b]` and `? - a\n  - b` give the
same key, `[a, b]`. The validator compares a key that is or holds an alias
with nothing, as the YAML test suite has `{&a [a]: 1, *a : 2}` as valid.

## Error handling

The primary error type is `YamlError`, with variants for I/O, parse errors
(with line and column), key-not-found (with available keys), type mismatch,
invalid index, and invalid operation.

Domain-specific error types are kept separate intentionally: `PathError` and
`PathParseError` for path access, `ValidationError` for schema validation,
`CustomTagError` for custom tags.

Some `YamlFile` convenience methods (`set`, `insert_at_index`,
`reorder_fields`) return `bool`, `false` meaning there was no suitable
document to act on and the file is unchanged.

Accessors that can hit a broken reference (an undefined alias, a merge
operand that is not a mapping) report it as an error rather than returning
the alias text or skipping it.

## Testing

Every test that mutates a tree checks the result after each mutation:

- `debug::validate_tree` checks the structural invariants from the `nodes`
  module docs: entries terminated by a NEWLINE, no stacked INDENTs, no
  doubled trailing NEWLINEs, one value node per KEY/VALUE/entry.
- `debug::roundtrip_ok` re-parses the text and checks that it parses without
  errors and renders back to the same text.
- In integration tests, the shared `assert_cst_ok(&doc)` and
  `assert_file_cst_ok(&file)` helpers run both. Unit tests use the
  test-only `debug::assert_cst_ok`, which takes any AST node. The fuzz and
  property tests have their own shared mutation-check helpers.
- Tests of operations that fail on purpose check the CST too, to show the
  failed operation left the tree intact.

These checks don't catch everything. A mutation can put a token in a
different node than the parser would (a NEWLINE under `DOCUMENT` rather than
in the last entry, a comment inside a `TAGGED_NODE`) and still pass both. For
mutations that touch document structure or move trivia around, also compare
`debug::tree_to_string` of the edited tree with that of a re-parse of its
text, and assert the exact output text with `assert_eq!`.

Tests compare with `assert_eq!` against exact expected values, not
`contains` or `matches!`.

## Code organization

- `lib.rs` - public API re-exports
- `yaml.rs` - `YamlFile` and core types
- `lex.rs` - lexer, token types (`SyntaxKind`)
- `parser/` - parser (block and flow collections, scalars)
- `parse.rs` - parse result types
- `nodes/` - AST node wrappers (`Document`, `Mapping`, `Sequence`, ...) and
  the CST invariants
- `as_yaml.rs` - `AsYaml` trait, `YamlNode` enum, `yaml_eq`, tagged
  collection types
- `value.rs` - `YamlValue`, an owned value tree not backed by the CST
- `scalar.rs` - `ScalarValue` and core schema typing
- `builder.rs` - fluent builder API
- `path.rs` - path access and alias policies
- `anchor_resolution.rs` - anchor/alias resolution, merge keys,
  `MergedMapping`
- `mapping_view.rs` - `MappingView` trait shared by `Mapping` and
  `MergedMapping`
- `migrate.rs` - YAML 1.1 to 1.2 migration
- `error.rs` - error types
- `schema.rs` - schema validation (Failsafe, JSON, Core)
- `custom_tags.rs` - custom tag registry
- `visitor.rs` - visitor pattern traversal
- `error_recovery.rs` - parse error recovery
- `validator.rs` - YAML spec validation
- `debug.rs` - tree visualization and CST checks

## Checklist

- Read the CST-invariants doc of the `nodes` module before editing mutation
  code
- Use `splice_children` for mutations - don't rebuild nodes
- Build CST nodes directly - never render text and re-parse it
- Put new spec checks in the validator, not the parser
- Methods take `&self`, not `&mut self` (interior mutability)
- Test that formatting is preserved (lossless round-trip)
- Call `debug::validate_tree` + `debug::roundtrip_ok` (or the
  `assert_cst_ok` / `assert_file_cst_ok` test helpers) after every
  mutation in new tests, and compare the tree with a re-parse when the
  mutation moves trivia or touches document structure
- Use `debug::print_tree()` to understand the CST structure
- Use `assert_eq!` with exact expected values in tests

## References

- [rowan](https://github.com/rust-analyzer/rowan)
- [YAML 1.2 Spec](https://yaml.org/spec/1.2.2/)
