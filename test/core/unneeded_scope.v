module top;
    parameter P = 3;
    wire [P + 1:0] x;
    wire [2 * P + 4:0] y, z;
    assign y = {x, x};
    generate
        if (1) begin : blk
            wire x;
        end
    endgenerate
    assign z = {blk.x, x};
endmodule
