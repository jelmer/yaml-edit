# TODO: YAML Specification Features Not Yet Supported

This document lists YAML 1.2 specification features that are not yet implemented in yaml-edit.

## High Priority (Common YAML Features)

### 1. Anchors & Aliases
- [ ] Anchor definitions (`&anchor_name`)
- [ ] Alias references (`*anchor_name`)
- [ ] Preserve anchors/aliases during editing
- [ ] Handle circular references safely

### 2. Multi-line Scalar Styles
- [ ] Literal block scalar (`|`) - proper parsing and preservation
- [ ] Folded block scalar (`>`) - proper parsing and preservation
- [ ] Block chomping indicators (`-`, `+`)
- [ ] Explicit indentation indicators (`|2`, `>3`)
- [ ] Block scalar content parsing with proper line break handling

### 3. Tags and Explicit Typing
- [ ] Local tags (`!custom`)
- [ ] Global tags (`!!str`, `!!int`, `!!float`, `!!bool`, `!!null`)
- [ ] Tag shorthand declarations
- [ ] Non-specific tags (`!` and `!!`)
- [ ] Preserve tags during editing
- [ ] Custom tag support

### 4. Escape Sequences in Strings
- [ ] Unicode escapes (`\xNN`, `\uNNNN`, `\UNNNNNNNN`)
- [ ] Control character escapes (`\n`, `\r`, `\t`, `\b`, etc.)
- [ ] Escaped quotes in quoted strings
- [ ] Line folding in double-quoted strings
- [ ] Escaped line breaks

## Medium Priority (Less Common Features)

### 5. Complex Mapping Keys
- [ ] Explicit key indicator (`?`)
- [ ] Non-scalar keys (sequences/mappings as keys)
- [ ] Multi-line keys
- [ ] Proper parsing and editing of complex keys

### 6. Directives
- [ ] YAML version directive (`%YAML 1.2`)
- [ ] TAG directives (`%TAG ! prefix`)
- [ ] Reserved directives
- [ ] Directive end marker (`---`)
- [ ] Preserve directives during editing

### 7. Special Collections
- [ ] Merge keys (`<<`) for key merging
- [ ] Sets (`!!set`)
- [ ] Ordered mappings (`!!omap`)
- [ ] Pairs (`!!pairs`)

### 8. Binary and Special Data Types
- [ ] Binary data (`!!binary` with base64 encoding)
- [ ] Timestamps (`!!timestamp`)
- [ ] Regular expressions
- [ ] Type casting/coercion

### 9. JSON Compatibility
- [ ] Full JSON compatibility (JSON is valid YAML)
- [ ] JSON-style escape sequences
- [ ] Proper number format compatibility

## Low Priority (Advanced/Rare Features)

### 10. Document Stream Features
- [ ] Document end marker (`...`) - currently only handles start marker
- [ ] Explicit document markers in single document
- [ ] Proper multi-document stream handling with all markers
- [ ] Document-level tags and directives

### 11. Comments
- [ ] Mid-line comments (currently only handles end-of-line)
- [ ] Comments between sequence/mapping items
- [ ] Comments in flow collections
- [ ] Preserve comment positioning more precisely

### 12. Whitespace and Formatting
- [ ] Tab character handling (YAML forbids tabs for indentation)
- [ ] Line length limits and folding
- [ ] Preserve exact scalar content indentation
- [ ] Handle various line break styles (LF, CRLF, CR)

### 13. Schema Support
- [ ] Failsafe schema
- [ ] JSON schema
- [ ] Core schema (default)
- [ ] Custom schema definitions

### 14. Error Handling
- [ ] Better error messages with line/column numbers
- [ ] Recovery from parse errors
- [ ] Validation against schema
- [ ] Warning for deprecated YAML 1.1 constructs

## Implementation Improvements

### Parser Improvements
- [ ] Proper indentation tracking
- [ ] Better empty value handling
- [ ] Improved flow collection parsing
- [ ] Handle edge cases in key detection

### API Enhancements
- [ ] Path-based access (implement `set_path` properly)
- [ ] Iterator improvements for sequences/mappings
- [ ] Builder pattern for creating YAML structures
- [ ] Visitor pattern for traversing YAML

### Performance
- [ ] Optimize tree traversal
- [ ] Lazy parsing for large files
- [ ] Streaming support for huge documents
- [ ] Memory usage optimization

## Testing
- [ ] Comprehensive test suite against YAML test suite
- [ ] Fuzzing tests
- [ ] Round-trip tests for all features
- [ ] Performance benchmarks

## Documentation
- [ ] API documentation for all public types
- [ ] Usage examples for each feature
- [ ] Migration guide from other YAML libraries
- [ ] Best practices guide

## Notes

The current implementation focuses on the most common YAML use cases:
- Simple scalars (strings, numbers, booleans, null)
- Basic mappings and sequences
- Block and flow styles
- Comments and formatting preservation
- Document markers (partial support)

Many advanced features like anchors/aliases, tags, and complex keys are rarely used in configuration files, which is our primary use case. These can be added incrementally based on user needs.