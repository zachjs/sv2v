module top;
    logic x;
    logic [2:0] y;
    for (genvar i = 0; i < 3; i++) begin : blk
        wire w;
        assign y[i] = w;
    end
    localparam type T = enum int { K = 0 };
    assign blk[K].w = 1;
    assign blk[$bits(x)].w = 1;
    assign blk[$bits(T) - 30].w = 1;
endmodule
