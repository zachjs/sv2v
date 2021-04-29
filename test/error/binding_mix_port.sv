// pattern: illegal mix of ordered and named port connections
module example(
    input a, b, c
);
endmodule
module top;
    wire a, b, c;
    example e(1, .*);
endmodule
