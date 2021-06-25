module Streamer(i, l1, r1, l2, r2);
    parameter IN_WIDTH = 0;
    parameter OUT_WIDTH = 0;
    parameter CHUNK_SIZE = 0;

    input wire [IN_WIDTH-1:0] i;
    output wire [OUT_WIDTH-1:0] l1;
    output wire [OUT_WIDTH-1:0] r1;
    output reg [OUT_WIDTH-1:0] l2;
    output reg [OUT_WIDTH-1:0] r2;

    if (IN_WIDTH <= OUT_WIDTH) begin
        wire [OUT_WIDTH-1:0] lA = {<<CHUNK_SIZE{i}};
        wire [OUT_WIDTH-1:0] rA = {>>CHUNK_SIZE{i}};
        wire [OUT_WIDTH-1:0] lB;
        wire [OUT_WIDTH-1:0] rB;
        assign lB = {<<CHUNK_SIZE{i}};
        assign rB = {>>CHUNK_SIZE{i}};
        assign l1 = lA == lB ? lA : 'x;
        assign r1 = rA == rB ? rA : 'x;
    end
    if (OUT_WIDTH <= IN_WIDTH) begin
        always @* {<<CHUNK_SIZE{l2}} = i;
        always @* {>>CHUNK_SIZE{r2}} = i;
    end
endmodule
