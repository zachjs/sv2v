// pattern: unknown bindings "w", "z" specified for port connections in instance "e" of module "example", 2 available \("x", "y"\)
module example(
    input x, y
);
endmodule
module top;
    example e(.w(1), .z(1'b0));
endmodule
