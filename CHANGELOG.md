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
