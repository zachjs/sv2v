module Tester;
    parameter IN_WIDTH = 0;
    parameter OUT_WIDTH = 0;
    parameter CHUNK_SIZE = 0;

    reg [IN_WIDTH-1:0] i;
    wire [OUT_WIDTH-1:0] l1, l2;
    wire [OUT_WIDTH-1:0] r1, r2;
    Streamer #(IN_WIDTH, OUT_WIDTH, CHUNK_SIZE)
        streamer(i, l1, r1, l2, r2);

    localparam DELAY = 8 * (CHUNK_SIZE + 8 * (OUT_WIDTH + 8 * IN_WIDTH));
    initial #DELAY;

    integer idx;
    initial begin
        for (idx = 0; idx < IN_WIDTH; idx = idx + 1) begin
            i = 1 << idx;
            #1 $display("INW=%0d OUTW=%0d CS=%0d i=%b l1=%b r1=%b l2=%b r2=%b",
                IN_WIDTH, OUT_WIDTH, CHUNK_SIZE, i, l1, r1, l2, r2);
        end
    end
endmodule

module top;
    generate
        genvar i, o, c;
        for (i = 1; i <= 8; i = i + 1)
            for (o = 1; o <= 8; o = o + 1)
                for (c = 1; c <= i; c = c + 1)
                    Tester #(i, o, c) tester();
    endgenerate
endmodule
