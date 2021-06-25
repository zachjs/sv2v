module Example(
    input wire signed [31:0] inp,
    output wire signed [31:0] out
);
    assign out = inp * 2;
endmodule
