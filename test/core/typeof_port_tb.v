module top;
    reg inp;
    wire out;
    Example e(inp, out);
    initial begin
        $monitor("%0d %b %b", $time, inp, out);
    end
endmodule
