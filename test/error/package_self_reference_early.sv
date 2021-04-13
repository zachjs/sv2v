// pattern: package "P" references undeclared local "P::Bar"
package P;
    localparam Foo = P::Bar;
    localparam Bar = 1;
endpackage
module top;
    import P::*;
    initial $display(Foo);
endmodule
