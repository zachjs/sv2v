module top;
    reg [8:0] inp;
    wire [2:0] out;
    child c(inp, out);
    initial begin
        $monitor("%2d %b %b", $time, inp, out);
        inp = 9'b00_101_0000; #1;
        inp = 9'b00_010_1111; #1;
        inp = 9'b01_111_0000; #1;
    end
endmodule
