package P;
    localparam Bar = 1;
    localparam Foo = P::Bar;
endpackage
module top;
    import P::*;
    initial $display(Foo);
endmodule
