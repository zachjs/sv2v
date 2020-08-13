`include "string_param.vh"

module Example(inp, out);
    parameter PATTERN = "whatever";
    parameter IN_WIDTH = $bits(PATTERN);
    localparam OUT_WIDTH = `COUNT_ONES(PATTERN);

    input wire [IN_WIDTH - 1:0] inp;
    output wire [OUT_WIDTH - 1:0] out;
    if (PATTERN[0])
        assign out[0] = inp[0];
    genvar j;
    for (j = 1; j < IN_WIDTH; j = j + 1)
        if (PATTERN[j])
            assign out[`COUNT_ONES(PATTERN[j - 1:0])] = inp[j];
endmodule
