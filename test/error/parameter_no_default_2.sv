// pattern: instance "bad" of "Example" is missing values for parameters without defaults: "Y"
module Example;
    parameter X = 1;
    parameter Y;
endmodule
module top;
    Example #(.Y(1)) good();
    Example #(1) bad();
endmodule
