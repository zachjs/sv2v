# Reid Long's tests

These tests are borrowed from Reid Long's [HDL Examples repository]. Early in
sv2v's development, Reid produced these examples to demonstrate how sv2v might
perform its conversions. sv2v does not necessarily convert code in the same way,
but its output should behave equivalently. Notably, sv2v does not create
`generate` blocks when flattening vectors with multiple packed dimensions, uses
`localparam`s rather than macros to convert `enum`s, and converts `struct`
pattern literals as concatenations, rather than multiple statements.

[HDL Examples repository]: https://bitbucket.org/ReidLong/hdl-examples

## Modifications

The following differences exist between the tests in this folder and their
corresponding versions in the source repository.

1. The `inline_concat` files were modified to remove a stray trailing semicolon.
   Though some tools allow for stray semicolons, `iverilog` does not.
2. `array.v` previously had a custom implementation of `$clog2`, which was
   removed.
3. `cache_request.sv` was modified to include a plain decimal literal to provide
   coverage beyond the unbased-unsized literals.
4. The `cache_request2` test is omitted. It was only an example for debugging a
   VCS-specific issue encountered with `cache_request`.
