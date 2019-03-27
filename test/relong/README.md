# relong Tests

These tests are borrowed from Reid Long's [HDL Examples
repository](https://bitbucket.org/ReidLong/hdl-examples). That repository was
intended to provide examples for how the conversions in this project could be
done.

The `inline_concat` files were modified to remove a stray trailing semicolon.

Each test case (say, "foo") is comprised of the following files:

1. `foo.sv`: original SystemVerilog
2. `foo.v`: hand-converted Verilog
3. `foo_tb.v`: basic testbench exercising the given modules

The SystemVerilog source file is converted to Verilog using sv2v, and then both
the converted file and the reference Verilog are simulated using Icarus Verilog.
This produces VCD files for each which are expected to match exactly, except for
the timestamp.
