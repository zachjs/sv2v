module top;
    localparam COUNT = 4;
    reg [COUNT * 3 - 1:0] inp;
    wire [COUNT - 1:0] out;
    mod #(COUNT) m(inp, out);
    initial begin : blk
        integer i;
        $monitor("%2d %b %b", $time, inp, out);
        for (i = 0; i < 2 ** (COUNT * 3); i = i + 1)
            #1 inp = i;
    end
endmodule
