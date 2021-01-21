// pattern: instance "bad" of "Example" is missing values for parameters without defaults: "Y"
interface Example;
    parameter X = 1;
    parameter Y;
endinterface
module top;
    Example #(.Y(1)) good();
    Example #(.Y()) bad();
endmodule
