package P;
    parameter X = 1;
    localparam Y = 2;
endpackage

module Example;
    import P::*;
    parameter Z = X * 7;
    initial $display("%0d %0d %0d", X, Y, Z);
endmodule
