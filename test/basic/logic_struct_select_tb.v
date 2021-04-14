module top;
    reg [1:0] sel;
    wire [7:0] out;
    example e(.sel, .out);
    integer i = 0;
    initial
        for (i = 0; i < 10; i = i + 1)
            #1 sel = i;
endmodule
