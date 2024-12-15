// pattern: unknown binding "z" specified for port connections in instance "e" of module "example", 2 available \("x", "y"\)
module example(
    input x, y
);
endmodule
module top;
    example e(.x(1'b1), .y(1'b0), .z(1'b0));
endmodule
