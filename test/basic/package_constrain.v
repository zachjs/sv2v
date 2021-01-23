module Example(inp);
    localparam W = 5;
    localparam unrelated = 1;
    input wire [W - 1:0] inp;
    initial $display("%b %0d %0d", inp, $bits(inp), unrelated);
endmodule
