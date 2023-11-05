module top;
    reg inp;
    wire out;
    Example e(inp, out);
    initial
        repeat(5) begin
            #1 inp = 0;
            #1 inp = 1;
        end
endmodule
