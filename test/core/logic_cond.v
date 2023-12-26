module Example(inp, out);
    parameter ENABLED = 1;
    localparam [0:0] DEFAULT = 1'b0;
    input wire inp;
    output reg out;
    generate
        if (ENABLED) begin
            reg start;
            always @(inp, start) out = inp;
            initial start = 0;
        end
        else
            initial out = DEFAULT;
    endgenerate
endmodule
