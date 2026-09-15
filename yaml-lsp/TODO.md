# TODO: Features from Red Hat YAML Language Server not yet implemented

This lists features present in the [Red Hat YAML Language Server](https://github.com/redhat-developer/yaml-language-server) that are not yet implemented in yaml-lsp.

## JSON Schema Support

- [ ] JSON Schema validation (drafts 04, 07, 2019-09, 2020-12)
- [ ] Schema association via modeline comments (`# yaml-language-server: $schema=<url>`)
- [ ] Schema association via `yaml.schemas` setting (glob-to-schema mapping)
- [ ] Schema Store integration (schemastore.org)
- [ ] Custom schema provider API for editor extensions
- [ ] Schema association notifications (dynamic registration)
- [ ] Kubernetes CRD store integration
- [ ] Special `"kubernetes"` schema key

## Completion / IntelliSense

- [ ] Property name completions from schema
- [ ] Enum value completions with descriptions
- [ ] Default value insertion from schema defaults
- [ ] Boolean value completions (`true`/`false`)
- [ ] Const value completions
- [ ] Array item template completions
- [ ] Default snippet support (with variable/tabstop expansion)
- [ ] Parent skeleton completions (objects with required properties)
- [ ] Modeline completion (`# yaml-language-server: $schema=`)
- [ ] Custom tag completions

## Hover

- [ ] Schema title and description display
- [ ] Enum values list with descriptions
- [ ] Examples rendered as YAML code blocks
- [ ] Source attribution / link to schema
- [ ] Merge key (`<<`) hover showing resolved merged content
- [ ] Type information from schema

## Document Formatting

- [ ] Full document formatting (Prettier-based or equivalent)
- [ ] `singleQuote` option
- [ ] `bracketSpacing` option
- [ ] `proseWrap` option
- [ ] `printWidth` option

## On-Type Formatting

- [ ] Auto-indent after `:`
- [ ] Auto-indent after `|` (block scalar)
- [ ] Array continuation (insert `- ` on newline)
- [ ] Nested array item indentation

## Document Symbols / Outline

- [ ] Flat symbol list
- [ ] Hierarchical / tree symbol structure

## Additional Code Actions

- [ ] Jump to associated schema
- [ ] Convert flow style to block style
- [ ] Convert string to block string (folded `>` / literal `|`)
- [ ] Fix key order (sort keys alphabetically)
- [ ] Fix enum/property mismatch (quick-fix for schema diagnostics)
- [ ] Convert quoted boolean to literal (`"true"` -> `true`)
- [ ] Delete unused anchor

## Code Lens

- [ ] Schema navigation lens (clickable schema reference at document start)

## Additional Diagnostics / Validation

- [ ] JSON Schema constraint validation
- [ ] Flow style enforcement (`yaml.style.flowMapping`, `yaml.style.flowSequence`)
- [ ] Key ordering enforcement (`yaml.keyOrdering`)
- [ ] Unused anchor detection
- [ ] Diagnostic suppression via comments (`# yaml-language-server-disable`)

## Folding Ranges

- [ ] Document-level folds (`---` separators)
- [ ] Array object folds
- [ ] Property-based folds (complex values)
- [ ] Multi-line string folds (block scalars)

## Selection Ranges

- [ ] Hierarchical selection expansion (innermost to outermost node)
- [ ] Quote/bracket inner selection ranges

## Document Links

- [ ] Clickable HTTP/HTTPS URLs
- [ ] File path references
- [ ] Schema `$ref` pointer links

## Custom Tags

- [ ] `yaml.customTags` configuration
- [ ] Tag-to-type mapping (scalar, sequence, mapping)
- [ ] Suppress "unknown tag" errors for registered tags

## YAML Version Selection

- [ ] `yaml.yamlVersion` setting (1.1 vs 1.2)

## Configuration

- [ ] `yaml.validate` toggle
- [ ] `yaml.hover` toggle
- [ ] `yaml.completion` toggle
- [ ] `yaml.format.enable` toggle
- [ ] `yaml.maxItemsComputed` limit
- [ ] `yaml.disableAdditionalProperties`
- [ ] `yaml.disableDefaultProperties`
- [ ] `yaml.suggest.parentSkeletonSelectedFirst`
- [ ] Proxy settings for schema downloads (`http.proxy`, `http.proxyStrictSSL`)
