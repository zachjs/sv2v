package P;
    localparam int W = 4;
    typedef logic [W - 1:0] T;
    typedef enum T {
        A = 4'b1010,
        B = 4'b0101
    } E;
endpackage

module top;
    initial $display("%d %d %d %b %b", $bits(P::A), $bits(P::T), $bits(P::E), P::A, P::B);
endmodule
