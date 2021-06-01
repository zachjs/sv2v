// pattern: unknown binding "R" specified for parameter overrides in instance "e" of "example"
module example;
    parameter P = 1;
    parameter Q = 1;
endmodule
module top;
    example #(.P(1), .R(2)) e();
endmodule
