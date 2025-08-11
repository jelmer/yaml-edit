# TODO: YAML Specification Features Not Yet Supported

This document lists YAML 1.2 specification features that are not yet implemented in yaml-edit.

## Known Bugs and Issues

### Critical Bugs
1. **Timestamp Parsing**: Timestamps with spaces fail to parse (e.g., `2001-12-14 21:59:43.10 -5`)
   - *Root cause*: Space-separated timestamps look like mapping syntax; needs special handling
2. **Special Collections**: `!!set`, `!!omap`, and `!!pairs` parse but lose their content
   - *Root cause*: Tags are preserved but special collection semantics not implemented
3. **Binary Data**: `!!binary` tag parses but the base64 content is lost
   - *Root cause*: Block scalar content after tags not properly associated

### Minor Issues
1. **Octal Numbers**: Legacy octal format (0755) works but modern format (0o755) may not be recognized correctly

## High Priority Features

### 1. Directives
- [ ] Reserved directives

### 2. Special Collections
- [ ] Sets (`!!set`) - *Currently broken: content is lost after parsing*
- [ ] Ordered mappings (`!!omap`) - *Currently broken: content is lost after parsing*
- [ ] Pairs (`!!pairs`) - *Currently broken: content is lost after parsing*

### 3. Binary and Special Data Types
- [ ] Binary data (`!!binary` with base64 encoding) - *Currently broken: content is lost*
- [ ] Timestamps (`!!timestamp`) - *Partially broken: space-separated format fails*
- [ ] Regular expressions
- [ ] Type casting/coercion

## Medium Priority Features

### 4. Comments
- [ ] Mid-line comments 
- [ ] Preserve comment positioning more precisely in complex structures

### 5. Schema Support
- [ ] Failsafe schema
- [ ] JSON schema
- [ ] Core schema (default)
- [ ] Custom schema definitions

### 6. Error Handling
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

## Additional Missing Features

### Number Formats
- [ ] Modern octal notation (`0o777`) - may not be recognized as octal
- [ ] Binary notation (`0b101010`)
- [ ] Hexadecimal with uppercase (`0xDEADBEEF`)
- [ ] Scientific notation (`6.02e23`)

### Plain Scalar Edge Cases
- [ ] URLs with colons (`http://example.com`) - colon causes truncation
- [ ] Email addresses (`user@example.com`) - @ causes truncation  
- [ ] Time values (`12:34:56`) - colons cause issues

### YAML 1.1 vs 1.2 Differences
- [ ] Boolean values (`yes`, `no`, `on`, `off`) - should be strings in 1.2 when quoted
- [ ] Octal notation changes (0755 vs 0o755)
- [ ] Merge key behavior differences