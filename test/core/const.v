module top;
    integer w = 11;
    reg [63:0] x = { 32'd11, 32'd12 };
    initial $display("%b %b %b %b", w, x, x[32+:32], x[0+:32]);
endmodule
