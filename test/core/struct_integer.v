module top;
    wire [32*3-1:0] s = {32'd1, 32'd2, 32'd3};
    initial #1 $display("%b %b %b %b", s, s[64+:32], s[32+:32], s[0+:32]);
endmodule
