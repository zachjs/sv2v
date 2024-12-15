// pattern: too many bindings specified for parameter overrides in instance "e" of module "example": 3 specified \(1, 2, 3\), but only 2 available \("P", "Q"\)
module example;
    parameter P = 1;
    parameter Q = 1;
endmodule
module top;
    example #(1, 2, 3) e();
endmodule
