## v0.0.12

### Breaking Changes

* Removed deprecated CLI flags `-d`/`-e`/`-i`, which have been aliased to
  `-D`/`-E`/`-I` with a warning since late 2019

### New Features

* `unique`, `unique0`, and `priority` case statements now produce corresponding
  `parallel_case` and `full_case` statement attributes
* Added support for attributes in unary, binary, and ternary expressions
* Added support for streaming concatenations within ternary expressions
* Added support for shadowing interface names with local typenames
* Added support for passing through `wait` statements

### Bug Fixes

* Fixed signed unsized literals with a leading 1 bit (e.g., `'sb1`, `'sh8f`)
  incorrectly sign-extending in size and type casts
* Fixed conflicting genvar names when inlining interfaces and modules that use
  them; all genvars are now given a design-wide unique name
* Fixed unconverted structs within explicit type casts
* Fixed byte order of strings in size casts
* Fixed unconverted multidimensional struct fields within dimension queries
* Fixed non-typenames (e.g., from packages or subsequent declarations)
  improperly shadowing the names of `struct` pattern fields
* Fixed shadowing of interface array indices passed to port connections
* Fixed failure to resolve typenames suffixed with dimensions in contexts
  permitting both types and expressions, e.g., `$bits(T[W-1:0])`
* Fixed an issue that prevented parsing tasks and functions with `inout` ports
* Fixed errant constant folding of shadowed non-trivial localparams
* Fixed conversion of function calls with no arguments passed to other functions
* Fixed certain non-ANSI style port declarations being incorrectly reported as
  incompatible

### Other Enhancements

* `always_comb` and `always_latch` now reliably execute at time zero
* Added error checking for unresolved typenames
* Added constant folding for `||` and `&&`
* `input reg` module ports are now converted to `input wire`
* `x | |y` and `x & &y` are now output as `x | (|y)` and `x & (&y)`

## v0.0.11

### New Features

* Added `-y`/`--libdir` for specifying library directories from which to
  automatically load modules and interfaces used in the design that are not
  found in the provided input files
* Added `--top` for pruning unneeded modules during conversion
* Added `--write path/to/dir/` for creating an output `.v` in the specified
  preexisting directory for each module in the converted result
* The `string` data type is now dropped from parameters and localparams
* Added support for passing through `sequence` and `property` declarations

### Bug Fixes

* Fixed crash when converting multi-dimensional arrays or arrays of structs or
  unions used in certain expressions involving unbased unsized literals
* Fixed module-level localparams being needlessly inlined when forming longest
  static prefixes, which could cause deep recursion and run out of memory on
  some designs
* Fixed overzealous removal of explicitly unconnected ports (e.g., `.a()`)
* Fixed an issue that left `always_comb`, `always_latch`, and `always_ff`
  unconverted when tagged with an attribute
* Fixed unneeded scoping of constant function calls used in type lookups
* `/*/` is no longer interpreted as a self-closing block comment, e.g.,
  `$display("a"/*/,"b"/* */);` previously printed "ab", but now prints "a"
* Fixed missing `begin`/`end` when disambiguating procedural branches tagged
  with an attribute
* Fixed keywords included in the "1364-2001" and "1364-2001-noconfig"
  `begin_keywords` version specifiers

### Other Enhancements

* Added elaboration for accesses to fields of struct constants, which can
  substantially improve conversion speed on some designs
* Added constant folding for comparisons involving string literals
* Port connection attributes (e.g., [pulp_soc.sv]) are now ignored with a
  warning rather than failing to parse
* Improved error message when specifying an extraneous named port connection
* Improved error message for an unfinished conditional directive, e.g., an
  `ifdef` with no `endif`
* Added checks for accidental usage of interface or module names as type names

[pulp_soc.sv]: https://github.com/pulp-platform/pulp_soc/blob/0573a85c/rtl/pulp_soc/pulp_soc.sv#L733

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
