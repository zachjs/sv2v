package P;
    localparam X = 1;
    localparam Y = 2;
    typedef enum {
        A = X,
        B = Y
    } Enum;
endpackage

module top;
    import P::*;
    initial $display("%0d %0d %0d", X, A, B);
endmodule
