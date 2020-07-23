module top;
    parameter WIDTH = 5;
    reg [WIDTH-1:0] x, y;
    if (WIDTH > 0)
        initial
            x = 0;
    initial
        y = 0;
endmodule
