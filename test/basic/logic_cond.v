module Example(inp, out);
    parameter ENABLED = 1;
    input wire inp;
    output reg out;
    generate
        if (ENABLED)
            always @* out = inp;
        else
            initial out = 0;
    endgenerate
endmodule
