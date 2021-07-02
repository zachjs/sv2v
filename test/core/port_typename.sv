typedef logic b_t;
module top(
    input a [1:0],
    input b_t b
);
    initial $display("%b %b %b %1d %1d", a[0], a[1], b, $bits(a), $bits(b));
endmodule
