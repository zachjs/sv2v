// pattern: required parameter "Y" in class "C" has not been provided
class C #(
    parameter Y
);
    localparam X = Y + 1;
endclass
module top;
    initial $display(C#()::X);
endmodule
