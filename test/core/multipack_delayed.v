module mod(inp, out);
    parameter COUNT = 1;
    input wire [COUNT * 3 - 1:0] inp;
    output reg [COUNT - 1:0] out;
    genvar i;
    generate
        for (i = 0; i < COUNT; i = i + 1)
            always @*
                if (inp[i * 3] | inp[i * 3 + 1])
                    out[i] = inp[i * 3 + 2] ^ inp[i * 3 + 1];
    endgenerate
endmodule
