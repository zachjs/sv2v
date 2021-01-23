// pattern: could not find package "PackageThatDoesNotExist"

package Wrap;
    import PackageThatDoesNotExist::*;
    localparam Foo = Bar;
endpackage

module top;
    import Wrap::*;
endmodule
