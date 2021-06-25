module top;
    localparam W = 4;
    localparam A = 4'b1010;
    localparam B = 4'b0101;
    initial $display("%d %d %b %b", W, W, A, B);
endmodule
