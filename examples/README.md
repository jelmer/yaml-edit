# YAML-Edit Examples

This directory contains examples demonstrating various features of the yaml-edit library.

## Examples

### Basic Operations
- `edit_example.rs` - Basic editing operations (get/set values)
- `working_edit.rs` - Real editing functionality test

### YAML Features
- `comprehensive_comment_test.rs` - Comment preservation across all contexts
- `yaml_spec_compliance.rs` - YAML 1.2 specification compliance
- `number_formats.rs` - Binary (0b), octal (0o), hex (0x) number formats
- `special_collections_debug.rs` - Special collections (!!set, !!omap, !!pairs)
- `anchors_comprehensive.rs` - Anchors and aliases (&, *)
- `merge_keys_example.rs` - Merge keys (<<)
- `multiline_detailed_test.rs` - Block scalars (|, >, |+, etc.)
- `document_stream_test.rs` - Multi-document streams (---, ...)
- `directive_example.rs` - YAML directives (%YAML, %TAG)
- `regex_yaml.rs` - Regular expression support with !!regex tag
- `regex_extraction.rs` - Extracting and using compiled Regex objects (requires 'regex' feature)

### Validation & Debugging
- `verify_all_fixes.rs` - Verification of critical bug fixes
- `hyphen_bug_fixed_demo.rs` - Hyphen handling demonstration
- `token_debug.rs` - Minimal token debugging example