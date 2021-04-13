// pattern: reference to "X" in parameterized class "C" requires explicit #()
class C #(
    parameter Y = 1
);
    localparam X = Y + 1;
endclass
module top;
    initial $display(C::X);
endmodule
