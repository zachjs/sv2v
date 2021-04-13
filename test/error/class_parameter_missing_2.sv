// pattern: required type parameter "Y" in class "C" has not been provided
class C #(
    parameter type Y
);
    localparam X = $bits(Y) + 1;
endclass
module top;
    initial $display(C#()::X);
endmodule
