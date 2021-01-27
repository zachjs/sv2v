module Example;
    localparam X = 1;
    localparam Y = 2;
    parameter Z = X * 7;
    initial $display("%0d %0d %0d", X, Y, Z);
endmodule
