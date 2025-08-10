# TODO: YAML Specification Features Not Yet Supported

This document lists YAML 1.2 specification features that are not yet implemented in yaml-edit.

## Recent Progress (Updated 2025-08-10)

Major features recently implemented:
- ✅ **Anchors & Aliases** - Full support for anchor definitions and alias references with preservation
- ✅ **Escape Sequences** - Complete implementation including Unicode escapes, control characters, and line folding
- ✅ **YAML Directives** - Support for YAML version and TAG directives with preservation
- ✅ **Merge Keys** - Full support for merge key syntax (`<<:`) with circular reference detection
- ✅ **Tags and Explicit Typing** - Complete support for local tags, global tags, non-specific tags, and custom tags with safe value extraction

## High Priority (Common YAML Features)

### 1. Anchors & Aliases
- [x] Anchor definitions (`&anchor_name`)
- [x] Alias references (`*anchor_name`)
- [x] Preserve anchors/aliases during editing
- [x] Handle circular references safely
- [x] Merge keys (`<<`) for key merging with full support

### 2. Multi-line Scalar Styles
- [x] Literal block scalar (`|`) - proper parsing and preservation
- [x] Folded block scalar (`>`) - proper parsing and preservation
- [x] Block chomping indicators (`-`, `+`)
- [x] Explicit indentation indicators (`|2`, `>3`)
- [x] Block scalar content parsing with proper line break handling

### 3. Tags and Explicit Typing
- [x] Local tags (`!custom`)
- [x] Global tags (`!!str`, `!!int`, `!!float`, `!!bool`, `!!null`)
- [x] Tag shorthand declarations (via TAG directives)
- [x] Non-specific tags (`!` and `!!`)
- [x] Preserve tags during editing
- [x] Custom tag support

### 4. Escape Sequences in Strings
- [x] Unicode escapes (`\xNN`, `\uNNNN`, `\UNNNNNNNN`)
- [x] Control character escapes (`\n`, `\r`, `\t`, `\b`, etc.)
- [x] Escaped quotes in quoted strings
- [x] Line folding in double-quoted strings
- [x] Escaped line breaks

## Medium Priority (Less Common Features)

### 5. Complex Mapping Keys
- [ ] Explicit key indicator (`?`)
- [ ] Non-scalar keys (sequences/mappings as keys)
- [ ] Multi-line keys
- [ ] Proper parsing and editing of complex keys

### 6. Directives
- [x] YAML version directive (`%YAML 1.2`)
- [x] TAG directives (`%TAG ! prefix`)
- [ ] Reserved directives
- [x] Directive end marker (`---`)
- [x] Preserve directives during editing

### 7. Special Collections
- [ ] Sets (`!!set`)
- [ ] Ordered mappings (`!!omap`)
- [ ] Pairs (`!!pairs`)

### 8. Binary and Special Data Types
- [ ] Binary data (`!!binary` with base64 encoding)
- [ ] Timestamps (`!!timestamp`)
- [ ] Regular expressions
- [ ] Type casting/coercion

### 9. JSON Compatibility
- [x] Full JSON compatibility (JSON is valid YAML)
- [x] JSON-style escape sequences
- [x] Proper number format compatibility

## Low Priority (Advanced/Rare Features)

### 10. Document Stream Features
- [x] Document end marker (`...`) - complete parsing and preservation
- [x] Explicit document markers in single document
- [x] Proper multi-document stream handling with all markers
- [x] Document-level tags and directives

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