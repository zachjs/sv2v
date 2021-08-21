## Unreleased

* Explicitly-sized number literals with non-zero bits exceeding the given width
  (e.g., `1'b11`, `3'sd8`, `2'o7`) are truncated and produce a warning, rather
  than yielding a cryptic error
* Unsized number literals exceeding the maximum width of 32 bits (e.g.,
  `'h1_ffff_ffff`, `4294967296`) are truncated and produce a warning, rather
  than being silently extended
  * Support for unsized number literals exceeding the standard-imposed 32-bit
    limit can be re-enabled with `--oversized-numbers`
* Number literals with leading zeroes which extend beyond the width of the
  literal (e.g., `1'b01`, `'h0_FFFF_FFFF`) now produce a warning
* Non-positive integer size casts are now detected and forbidden
* Negative indices in struct pattern literals are now detected and forbidden
* Fix parsing of alternate spacings of `@(*)`
* Tolerate escaped vendor block comments in macro bodies
* Support deferred immediate assertion statements
* Apply implicit port directions to tasks and functions
* Support bare delay controls with real number delays
* Fix parsing of sized ports with implicit directions
* Ensure arrays used in nested ternary expressions are properly flattened
* Support parameters which use a type-of as the data type
* Support typed valued parameters declared in parameter port lists without
  explicitly providing a leading `parameter` or `localparam` marker

## v0.0.8

Future releases will have complete change logs.
