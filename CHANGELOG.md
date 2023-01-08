## Unreleased

### Bug Fixes

* Fixed module-level localparams being needlessly inlined when forming longest
  static prefixes, which could cause deep recursion and run out of memory on
  some designs
* Fixed unneeded scoping of constant function calls used in type lookups

### New Features

* `string` data type is now dropped from parameters and localparams

### Other Enhancements

* Added elaboration for accesses to fields of struct constants, which can
  substantially improve performance on some designs
* Added constant folding for comparisons involving string literals

## v0.0.10

### Breaking Changes

* `--write adjacent` no longer forbids overwriting existing generated files

### New Features

* Added support for assignments within expressions (e.g., `x = ++y;`)
* Added support for excluding the conversion of unbased unsized literals (e.g.,
  `'1`, `'x`) via `--exclude UnbasedUniszed`
* Added support for enumerated type ranges (e.g., `enum { X[3:5] }`)
* Added support for complex event expressions (e.g., `@(x ^ y)`)
* Added support for the SystemVerilog `edge` event
* Added support for cycle delay ranges in assertion sequence expressions
* Added support for procedural continuous assignments (`assign`/`deassign` and
  `force`/`release`)
* Added conversion for `do` `while` loops
* Added support for hierarchical calls to functions with no inputs
* Added support for passing through DPI imports and exports
* Added support for passing through functions with output ports
* Extended applicability of simplified Yosys-compatible `for` loop elaboration

### Other Enhancements

* Certain errors raised during conversion now also provide hierarchical and
  approximate source location information to help locate the error

### Bug Fixes

* Fixed inadvertent design behavior changes caused by constant folding removing
  intentional width-extending operations such as `+ 0` and `* 1`
* Fixed forced conversion to `reg` of data sensed in an edge-controlled
  procedural assignment
* `always_comb` and `always_latch` now generate explicit sensitivity lists where
  necessary because of calls to functions which reference non-local data
* Fixed signed `struct` fields being converted to unsigned expressions when
  accessed directly
* Fixed conversion of casts using structs containing multi-dimensional fields
* Fixed incorrect name resolution conflicts raised during interface inlining
* Fixed handling of interface instances which shadow other declarations
* Fixed names like `<pkg>_<name>` being shadowed by elaborated packages

## v0.0.9

### Breaking Changes

* Unsized number literals exceeding the maximum width of 32 bits (e.g.,
  `'h1_ffff_ffff`, `4294967296`) are now truncated and produce a warning, rather
  than being silently extended
  * Support for unsized number literals exceeding the standard-imposed 32-bit
    limit can be re-enabled with `--oversized-numbers`
* Input source files are now decoded as UTF-8 on all platforms, with transcoding
  failures tolerated, enabling reading files encoded using other ASCII supersets
  (e.g., Latin-1)

### New Features

* Added support for non-ANSI style port declarations where the port declaration
  is separate from the corresponding net or variable declaration
* Added support for typed value parameters declared in parameter port lists
  without explicitly providing a leading `parameter` or `localparam` marker
* Added support for tasks and functions with implicit port directions
* Added support for parameters which use a type-of as the data type
* Added support for bare delay controls with real number delays
* Added support for deferred immediate assertions

### Other Enhancements

* Explicitly-sized number literals with non-zero bits exceeding the given width
  (e.g., `1'b11`, `3'sd8`, `2'o7`) are now truncated and produce a warning,
  rather than yielding a cryptic error
* Number literals with leading zeroes which extend beyond the width of the
  literal (e.g., `1'b01`, `'h0_FFFF_FFFF`) now produce a warning
* Non-positive integer size casts are now detected and forbidden
* Negative indices in struct pattern literals are now detected and forbidden
* Escaped vendor block comments in macro bodies are now tolerated
* Illegal bit-selects and part-selects of scalar struct fields are now detected
  and forbidden, rather than yielding an internal assertion failure

### Bug Fixes

* Fixed parsing of sized ports with implicit directions
* Fixed flattening of arrays used in nested ternary expressions
* Fixed preprocessing of line comments which are neither preceded nor followed
  by whitespace except for the newline which terminates the comment
* Fixed parsing of alternate spacings of `@(*)`
* Fixed conversion of interface-based typedefs when used with explicit modports,
  unpacked arrays, or in designs with multi-dimensional instances
* Fixed conversion of module-scoped references to modports
* Fixed conversion of references to modports nested within types in expressions
* Fixed assertion removal in verbose mode causing orphaned statements

## v0.0.8

Future releases will have complete change logs.
