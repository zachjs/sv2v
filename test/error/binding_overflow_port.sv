// pattern: too many bindings specified for port connections in instance "e" of "example"
module example(
    input x, y
);
endmodule
module top;
    example e(1'b1, 1'b0, 1'b0);
endmodule
