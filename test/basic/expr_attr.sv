module top;
    wire a, b, c, d, e;
    assign a = b ? (* ternary *) ~^ (* unary *) c : d & (* binary *) e;
endmodule
