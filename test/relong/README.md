# relong Tests

These tests are borrowed from Reid Long's [HDL Examples
repository](https://bitbucket.org/ReidLong/hdl-examples). That repository was
intended to provide examples for how the conversions in this project could be
done. sv2v does not necessarily convert code as demonstrated in the examples.
Notably, sv2v does not create `generate` blocks when converted vectors with
multiple packed dimensions, uses `localparam`s rather than macros for `enum`
conversion, and converts `struct` literals to concatenations, rather than
multiple statements.

Each test case (say, "foo") is comprised of the following files:

1. `foo.sv`: original SystemVerilog
2. `foo.v`: hand-converted Verilog
3. `foo_tb.v`: basic testbench exercising the given modules

The SystemVerilog source file is converted to Verilog using sv2v, and then both
the converted file and the reference Verilog are simulated using Icarus Verilog.
This produces VCD files for each which are expected to match exactly, except for
the timestamp.

## Modifications

The following differences exist between the tests in this folder and their
corresponding versions in the source repository.

1. The `inline_concat` files were modified to remove a stray trailing semicolon.
   Though some tools allow for stray semicolons, `iverilog` does not.
2. `array.v` previously had a custom implementation of `$clog2`, which was
   removed.
3. `cache_request.sv` was modified to include a plain decimal literal to ensure
   coverage beyond the unbased-unsized literals.
4. The `cache_request2` test is omitted. It was only an example for debugging a
   VCS-specific issue encountered with `cache_request`.
