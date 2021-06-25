package PkgA;
    localparam Foo = 1;
endpackage
package PkgB;
    export PkgA::*;
    localparam Bar = 2;
endpackage
module top;
    initial $display(PkgB::Bar);
endmodule
