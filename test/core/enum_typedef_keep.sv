module top;
    localparam int W = 4;
    typedef logic [W - 1:0] T;
    typedef enum T {
        A = 4'b1010,
        B = 4'b0101
    } E;
    initial $display("%d %d %b %b", $bits(A), $bits(B), A, B);
endmodule
