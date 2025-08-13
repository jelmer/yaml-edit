# TODO: yaml-edit Roadmap

A Rust library for parsing and editing YAML files while preserving formatting, comments, and structure.

## Summary

yaml-edit has made significant progress with core YAML 1.2 features. The parser now correctly handles:
- ✅ All critical parsing bugs (timestamps, binary data, special collections)
- ✅ Complex plain scalars (URLs, emails, time values)
- ✅ Block scalars with proper indentation
- ✅ Flow collections and nested structures
- ✅ Comments preservation in most contexts
- ✅ Basic editing API while preserving structure
- ✅ Anchors, aliases, and merge keys

This document tracks remaining features and improvements needed for full YAML 1.2 compliance.

## ✅ Recently Fixed Issues

### Critical Bugs (FIXED)
1. **Timestamp Parsing**: ✅ FIXED - Timestamps with spaces now parse correctly (e.g., `2001-12-14 21:59:43.10 -5`)
2. **Special Collections**: ✅ FIXED - `!!set`, `!!omap`, and `!!pairs` now preserve their content correctly
3. **Binary Data**: ✅ FIXED - `!!binary` tag now properly preserves base64 content

## 🎯 Next Priority Items

These are the most impactful features to implement next:

1. **Path-based API** - Enable `yaml.get("server.host")` and `yaml.set("server.port", 8080)`
2. **Schema support** - Validate and enforce YAML schemas  
3. **Better error recovery** - Continue parsing after errors with meaningful messages
4. **Performance optimizations** - Streaming and lazy parsing for large files
5. **Regular expressions** - `!!regex` tag support

## Known Issues

### Minor Issues
1. **Complex key formatting** - Explicit key indicator (`?`) may not preserve exact formatting
2. **Mid-line comments in flow** - Comments inside flow collections not fully supported

## Feature Categories

### 🔧 API Enhancements
- [ ] **Path-based access** - `yaml.get("server.host")`, `yaml.set("server.port", 8080)`
- [ ] **Builder pattern** - Fluent API for creating YAML structures
- [ ] **Iterator improvements** - Better traversal of sequences/mappings
- [ ] **Visitor pattern** - Walk YAML trees with callbacks

### 📐 Schema Support
- [ ] **Failsafe schema** - Minimal type set
- [ ] **JSON schema** - JSON-compatible types only
- [ ] **Core schema** - Full YAML 1.2 type set
- [ ] **Custom schemas** - User-defined type validation

### 🔢 Number Formats
- [x] ~~Binary notation~~ - `0b101010` now supported
- [x] ~~Modern octal~~ - `0o777` now supported (legacy `0755` still works)
- [x] ~~Hexadecimal~~ - Both `0xFF` and `0xff` work
- [x] ~~Scientific notation~~ - `6.02e23` works

### 🎯 Advanced Types
- [ ] **Regular expressions** - `!!regex` tag support
- [ ] **Type coercion** - Automatic type conversion rules
- [ ] **Custom tags** - User-defined type handlers

### ⚡ Performance
- [ ] **Streaming parser** - Process huge files without loading all into memory
- [ ] **Lazy evaluation** - Parse only accessed portions
- [ ] **Memory optimization** - Reduce allocations and copies
- [ ] **Parallel processing** - Multi-threaded parsing for large documents

### 🛠️ Developer Experience
- [ ] **Better errors** - Line/column numbers, recovery suggestions
- [ ] **Validation** - Schema validation with helpful messages
- [ ] **Debugging tools** - AST visualization, parse tree dumps
- [ ] **YAML 1.1 warnings** - Detect deprecated constructs

### 📚 Documentation & Testing
- [ ] **Complete API docs** - All public types documented
- [ ] **Usage examples** - Real-world scenarios
- [ ] **YAML Test Suite** - Official compliance tests
- [ ] **Fuzzing** - Robustness testing
- [ ] **Benchmarks** - Performance metrics