// pattern: could not find "ItemThatDoesNotExist" in package "Pkg"

package Pkg;
    localparam Foo = 1;
endpackage

package Wrap;
    localparam Foo = Pkg::ItemThatDoesNotExist;
endpackage

module top;
    import Wrap::*;
endmodule
