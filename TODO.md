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
4. **Error Recovery**: ✅ FIXED - Infinite loops in error recovery eliminated, proper loop termination implemented

## 🎯 Next Priority Items

These are the most impactful features to implement next:

1. **Path-based API** - Enable `yaml.get("server.host")` and `yaml.set("server.port", 8080)`
2. ✅ **Schema support** - Validate and enforce YAML schemas (IMPLEMENTED - failsafe, JSON, core, custom schemas)
3. ✅ **Type coercion improvements** - More sophisticated automatic type conversion rules (IMPLEMENTED - enhanced cross-type coercion, thousand separators, expanded boolean/null recognition, timestamp formats)
4. **Performance optimizations** - Streaming and lazy parsing for large files

## Known Issues

### Minor Issues
1. **Complex key formatting** - Explicit key indicator (`?`) may not preserve exact formatting
2. **Mid-line comments in flow** - Comments inside flow collections not fully supported

## Feature Categories

### 🔧 API Enhancements
- [ ] **Path-based access** - `yaml.get("server.host")`, `yaml.set("server.port", 8080)`
- [x] ✅ **Builder pattern** - Fluent API for creating YAML structures (IMPLEMENTED - YamlBuilder with fluent API)
- [ ] **Iterator improvements** - Better traversal of sequences/mappings
- [x] ✅ **Visitor pattern** - Walk YAML trees with callbacks (IMPLEMENTED - YamlVisitor trait with accept methods)

### 📐 Schema Support
- [x] ✅ **Failsafe schema** - Minimal type set (strings, sequences, mappings only)
- [x] ✅ **JSON schema** - JSON-compatible types only (string, number, boolean, null, array, object)  
- [x] ✅ **Core schema** - Full YAML 1.2 type set (includes timestamps, regex, binary)
- [x] ✅ **Schema validation API** - `doc.validate_schema()`, `doc.validate_failsafe()`, etc.
- [x] ✅ **Type coercion** - Non-strict mode allows automatic type conversion
- [x] ✅ **Validation errors** - Detailed error messages with paths
- [x] ✅ **Custom schemas** - User-defined type validation and constraints with custom validators

### 🔢 Number Formats
- [x] ~~Binary notation~~ - `0b101010` now supported
- [x] ~~Modern octal~~ - `0o777` now supported (legacy `0755` still works)
- [x] ~~Hexadecimal~~ - Both `0xFF` and `0xff` work
- [x] ~~Scientific notation~~ - `6.02e23` works

### 🎯 Advanced Types
- [x] ~~Regular expressions~~ - `!!regex` tag support now implemented
- [x] ✅ **Type coercion** - Automatic type conversion rules (IMPLEMENTED - sophisticated cross-type coercion with enhanced parsing)
- [x] **Custom tags** - User-defined type handlers

### ⚡ Performance
- [ ] **Lazy evaluation** - Parse only accessed portions
- [ ] **Memory optimization** - Reduce allocations and copies
- [ ] **Parallel processing** - Multi-threaded parsing for large documents

### 🛠️ Developer Experience
- [x] ✅ **Better error recovery** - Robust parsing that continues after errors
- [x] ✅ **Validation** - Schema validation with helpful messages (failsafe, JSON, core)
- [x] ✅ **Detailed error messages** - Context-aware error reporting with suggestions
- [ ] **Line/column numbers** - Precise error locations in source
- [ ] **Debugging tools** - AST visualization, parse tree dumps  
- [ ] **YAML 1.1 warnings** - Detect deprecated constructs

### 📚 Documentation & Testing
- [ ] **Complete API docs** - All public types documented
- [ ] **Usage examples** - Real-world scenarios
- [ ] **YAML Test Suite** - Official compliance tests
- [ ] **Fuzzing** - Robustness testing
- [ ] **Benchmarks** - Performance metrics
