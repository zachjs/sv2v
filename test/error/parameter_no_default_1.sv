// pattern: instance "e" of "Example" is missing values for parameters without defaults: "X"
module Example;
    parameter type X;
endmodule
module top;
    Example e();
endmodule
