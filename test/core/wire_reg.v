module top(inp, out);
    input wire inp;
    reg data;
    always @* data = inp;
    output reg [1:0] out;

    parameter ON = 1;
    generate
        if (ON) begin : blk
            always @* out[0] = data;
            always @* out[1] = data;
        end
    endgenerate
endmodule
