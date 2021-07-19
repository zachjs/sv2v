module top;
    localparam X = $clog2(100);
    localparam Y = X + 1;
    localparam Z = Y + 1;
    localparam V = Y + 1;
    initial $display("%0d %0d", V, X);
endmodule
