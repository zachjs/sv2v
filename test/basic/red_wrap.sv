module top;
    wire a, b, x, y;
    assign x = a | |b;
    assign y = a & &b;
endmodule
