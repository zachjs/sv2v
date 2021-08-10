module top;
    localparam X = NUM;
    localparam [63:0] Y = X;
    initial $display("%0d %b %b", $bits(X), X, Y);
endmodule
