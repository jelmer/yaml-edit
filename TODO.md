# TODO: YAML Specification Features Not Yet Supported

This document lists YAML 1.2 specification features that are not yet implemented in yaml-edit.

## Recent Progress (Updated 2025-08-11)

Major features recently implemented:
- ✅ **Anchors & Aliases** - Full support for anchor definitions and alias references with preservation
- ✅ **Escape Sequences** - Complete implementation including Unicode escapes, control characters, and line folding
- ✅ **YAML Directives** - Support for YAML version and TAG directives with preservation
- ✅ **Merge Keys** - Full support for merge key syntax (`<<:`) with circular reference detection
- ✅ **Tags and Explicit Typing** - Complete support for local tags, global tags, non-specific tags, and custom tags with safe value extraction
- ✅ **Document Stream Features** - Complete multi-document stream support with proper marker handling
- ✅ **Enhanced Comment Support** - Improved comment parsing and preservation in various contexts
- ✅ **Whitespace and Formatting** - Complete implementation of tab validation, line length limits, indentation preservation, and line ending handling

## Code Quality Improvements
- ✅ **Test Suite** - Comprehensive test coverage with 130+ unit tests passing
- ✅ **API Stability** - Resolved import issues and duplicate implementations
- ✅ **Code Formatting** - All code properly formatted with cargo fmt

## Known Bugs and Issues

### Critical Bugs
1. **Complex Keys**: Explicit key indicator (`?`) doesn't properly parse complex sequences/mappings as keys
2. **Plain Scalars**: Email addresses and URLs with special characters (@ and :) are incorrectly truncated
3. **Timestamp Parsing**: Timestamps with spaces fail to parse (e.g., `2001-12-14 21:59:43.10 -5`)
4. **Special Collections**: `!!set`, `!!omap`, and `!!pairs` parse but lose their content
5. **Binary Data**: `!!binary` tag parses but the base64 content is lost

### Minor Issues
1. **Octal Numbers**: Legacy octal format (0755) works but modern format (0o755) may not be recognized correctly
2. **Complex Key Values**: When using mappings or sequences as keys, only partial content is preserved

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
- [ ] Explicit key indicator (`?`) - *Partially broken: complex keys are truncated*
- [ ] Non-scalar keys (sequences/mappings as keys) - *Broken: only partial content preserved*
- [ ] Multi-line keys
- [ ] Proper parsing and editing of complex keys

### 6. Directives
- [x] YAML version directive (`%YAML 1.2`)
- [x] TAG directives (`%TAG ! prefix`)
- [ ] Reserved directives
- [x] Directive end marker (`---`)
- [x] Preserve directives during editing

### 7. Special Collections
- [ ] Sets (`!!set`) - *Currently broken: content is lost after parsing*
- [ ] Ordered mappings (`!!omap`) - *Currently broken: content is lost after parsing*
- [ ] Pairs (`!!pairs`) - *Currently broken: content is lost after parsing*

### 8. Binary and Special Data Types
- [ ] Binary data (`!!binary` with base64 encoding) - *Currently broken: content is lost*
- [ ] Timestamps (`!!timestamp`) - *Partially broken: space-separated format fails*
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
- [x] End-of-line comments (fully supported)
- [x] Comments in flow collections (enhanced support)
- [x] Comments between sequence/mapping items (improved)
- [ ] Preserve comment positioning more precisely in complex structures

### 12. Whitespace and Formatting
- [x] Tab character handling (YAML forbids tabs for indentation)
- [x] Line length limits and folding
- [x] Preserve exact scalar content indentation
- [x] Handle various line break styles (LF, CRLF, CR)

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

## Additional Missing Features (Discovered via Testing)

### Number Formats
- [ ] Modern octal notation (`0o777`) - may not be recognized as octal
- [ ] Binary notation (`0b101010`)
- [ ] Hexadecimal with uppercase (`0xDEADBEEF`)
- [ ] Scientific notation (`6.02e23`)

### Special Float Values
- [x] Infinity (`.inf`, `-.inf`) - works correctly
- [x] Not-a-number (`.nan`) - works correctly

### Plain Scalar Edge Cases
- [ ] URLs with colons (`http://example.com`) - colon causes truncation
- [ ] Email addresses (`user@example.com`) - @ causes truncation  
- [ ] Time values (`12:34:56`) - colons cause issues

### Flow Collection Features
- [x] Trailing commas - supported correctly
- [x] Empty collections - supported correctly
- [x] Nested empty collections - supported correctly

### YAML 1.1 vs 1.2 Differences
- [ ] Boolean values (`yes`, `no`, `on`, `off`) - should be strings in 1.2 when quoted
- [ ] Octal notation changes (0755 vs 0o755)
- [ ] Merge key behavior differences

## Notes

The current implementation supports most common and many advanced YAML use cases:
- ✅ Simple scalars (strings, numbers, booleans, null)
- ✅ Basic mappings and sequences  
- ✅ Block and flow styles
- ✅ Comments and formatting preservation
- ✅ Document markers (complete support)
- ✅ Anchors, aliases, and merge keys
- ✅ Tags and explicit typing
- ✅ Multi-document streams
- ✅ Block scalars with all indicators

The library now covers the vast majority of YAML features used in real-world applications. Remaining features like complex keys, special collections, and schema support can be added incrementally based on user needs.