// pattern: illegal mix of ordered and named parameter overrides
module example #(
    parameter P = 1, Q = 2
) (
    input a, b, c
);
endmodule
module top;
    wire a, b, c;
    example #(1, .Q(2)) e(.*);
endmodule
