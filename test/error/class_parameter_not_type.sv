// pattern: cannot override parameter "Y" in class "C" with type logic
class C #(
    parameter Y
);
    localparam X = Y + 1;
endclass
module top;
    initial $display(C#(logic)::X);
endmodule
