module top(
    input [1:0] a,
    input wire b
);
    initial $display("%d %d %1d %1d", a, b, $bits(a), $bits(b));
endmodule
