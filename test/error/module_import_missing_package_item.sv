// pattern: could not find "ItemThatDoesNotExist" in package "Pkg"

package Pkg;
    localparam Foo = 1;
endpackage

module top;
    import Pkg::ItemThatDoesNotExist;
endmodule
