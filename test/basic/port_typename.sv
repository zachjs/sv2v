typedef wire b_t;
module top(
    input a [1:0],
    input b_t b
);
    initial $display("%d %d %1d %1d", a, b, $bits(a), $bits(b));
endmodule
