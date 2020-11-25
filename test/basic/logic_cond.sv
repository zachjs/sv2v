module Example(inp, out);
    parameter ENABLED = 1;
    input logic inp;
    output logic out;
    if (ENABLED)
        always_comb out = inp;
    else
        assign out = '0;
endmodule
