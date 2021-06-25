`include "string_param.vh"

module Example(inp, out);
    parameter PATTERN = "whatever";
    parameter UNUSED = 0;
    localparam IN_WIDTH = $bits(PATTERN);
    localparam OUT_WIDTH = `COUNT_ONES(PATTERN);

    input [IN_WIDTH - 1:0] inp;
    output [OUT_WIDTH - 1:0] out;
    if (PATTERN[0])
        assign out[0] = inp[0];
    for (genvar j = 1; j < IN_WIDTH; ++j)
        if (PATTERN[j])
            assign out[`COUNT_ONES(PATTERN[j - 1:0])] = inp[j];
endmodule
