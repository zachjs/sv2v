module top(inp, out);
    input wire inp;
    reg data;
    always @* data = inp;
    output logic [1:0] out;

    parameter ON = 1;
    generate
        if (ON) begin : blk
            assign out[0] = data;
            always @* out[1] = data;
        end
    endgenerate
endmodule
