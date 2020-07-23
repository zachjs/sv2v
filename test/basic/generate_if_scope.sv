module top;
    parameter WIDTH = 5;
    logic [WIDTH-1:0] x, y;
    if (WIDTH > 0)
        initial
            x = WIDTH'(0);
    initial
        y = WIDTH'(0);
endmodule
