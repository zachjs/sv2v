module top;
    reg signed [31:0] inp;
    wire signed [31:0] out;
    Example e(inp, out);
    initial begin
        #1 inp = 1;
        #1 inp = 5;
        #1 inp = 10;
        #1 inp = 7;
    end
endmodule
