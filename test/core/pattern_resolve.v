module top;
    parameter [95:0] W = {32'd3, 32'd5, 32'd7};
    localparam Z = W[95:64];
    localparam Y = W[31:0];
    initial $display(W, W[95:64], W[63:32], W[31:0], Z, Y);

    reg [55:0] a, b, c;
    initial begin
        a = {8'd2, 16'd1, 32'd3};
        b = {8'd1, 16'd3, 32'd3};
        c = {8'd4, 16'hFFFF, 32'd4};
        `define DUMP(v) $display("%b %b %b", v[55:48], v[47:32], v[31:0]);
        `DUMP(a) `DUMP(b) `DUMP(c)
    end
endmodule
