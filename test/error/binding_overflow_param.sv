// pattern: too many bindings specified for parameter overrides in instance "e" of "example"
module example;
    parameter P = 1;
    parameter Q = 1;
endmodule
module top;
    example #(1, 2, 3) e();
endmodule
