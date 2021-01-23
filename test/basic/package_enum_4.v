module top;
    localparam X = 1;
    localparam Y = 2;
    localparam A = X;
    localparam B = Y;
    initial $display("%0d %0d %0d", X, A, B);
endmodule
