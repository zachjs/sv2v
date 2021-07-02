module top(
    input wire [1:0] a,
    input wire b
);
    initial $display("%b %b %b %1d %1d", a[0], a[1], b, $bits(a), $bits(b));
endmodule
