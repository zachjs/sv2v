module top;
    localparam [0:127] B = { 32'h1, 32'h2, 32'h3, 32'h4 };
    localparam [0:31] C = { 8'h1, 8'h2, 8'h3, 8'h4 };
    localparam [0:3] D = { 1'h1, 1'h0, 1'h1, 1'h0 };
    initial begin
        $display("%b %2d %2d", B, $bits(B), 32);
        $display("%b %2d %2d", C, $bits(C), 8);
        $display("%b %2d %2d", D, $bits(D), 1);
    end
endmodule
