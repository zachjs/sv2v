module Streamer(i, l1, r1, l2, r2);
    parameter IN_WIDTH = 0;
    parameter OUT_WIDTH = 0;
    parameter CHUNK_SIZE = 0;

    input wire [IN_WIDTH-1:0] i;
    output wire [OUT_WIDTH-1:0] l1;
    output wire [OUT_WIDTH-1:0] r1;
    output reg [OUT_WIDTH-1:0] l2;
    output reg [OUT_WIDTH-1:0] r2;

    function [IN_WIDTH-1:0] stream_left;
        input [IN_WIDTH-1:0] inp;
        integer idx;
        localparam remainder = IN_WIDTH % CHUNK_SIZE;
        localparam remainder_fake = remainder ? remainder : 1;
        begin
            for (idx = 0; idx + CHUNK_SIZE <= IN_WIDTH; idx = idx + CHUNK_SIZE)
                stream_left[IN_WIDTH - idx - 1 -: CHUNK_SIZE] = inp[idx+:CHUNK_SIZE];
            if (remainder)
                stream_left[0+:remainder_fake] = inp[idx+:remainder_fake];
        end
    endfunction

    function [OUT_WIDTH-1:0] pad;
        input [IN_WIDTH-1:0] inp;
        pad = IN_WIDTH > OUT_WIDTH
            ? inp >> IN_WIDTH - OUT_WIDTH
            : inp << OUT_WIDTH - IN_WIDTH
            ;
    endfunction

    generate
        if (IN_WIDTH <= OUT_WIDTH) begin
            assign l1 = pad(stream_left(i));
            assign r1 = pad(i);
        end
        if (OUT_WIDTH <= IN_WIDTH) begin
            always @* l2 = pad(stream_left(i));
            always @* r2 = pad(i);
        end
    endgenerate
endmodule
