module Example(flag, out);
    output wire [1:0] out;
    input wire flag;
    assign out = flag ? 2'b10 : 2'b11;
endmodule
