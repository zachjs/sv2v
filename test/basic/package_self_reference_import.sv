package Q;
    localparam Bar = 1;
endpackage
package P;
    import Q::Bar;
    localparam Foo = P::Bar;
endpackage
module top;
    import P::*;
    initial $display(Foo);
endmodule
