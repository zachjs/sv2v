// pattern: cannot override type parameter "Y" in class "C" with expression 1
class C #(
    parameter type Y
);
    localparam X = $bits(Y) + 1;
endclass
module top;
    initial $display(C#(1)::X);
endmodule
