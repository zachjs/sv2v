module Example(inp, out);
    parameter ENABLED = 1;
    localparam [0:0] DEFAULT = 1'b0;
    input logic inp;
    output logic out;
    if (ENABLED)
        always_comb out = inp;
    else
        assign out = DEFAULT;
endmodule
